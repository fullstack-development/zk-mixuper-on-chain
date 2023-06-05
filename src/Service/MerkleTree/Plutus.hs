{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Implementation based on https://github.com/input-output-hk/hydra/tree/master/plutus-merkle-tree
module Service.MerkleTree.Plutus where

import Ext.PlutusTx.List (replicate)
import Ext.PlutusTx.Numeric ((^))
import GHC.Generics (Generic)
import qualified PlutusTx
import PlutusTx.Prelude hiding (toList)
import qualified Prelude as Haskell

-- | A type for representing hash digests.
type Hash = BuiltinByteString

-- | Computes a SHA-256 hash of a given 'BuiltinByteString' message.
hash :: BuiltinByteString -> Hash
hash = sha2_256
{-# INLINEABLE hash #-}

{- | Combines two hashes digest into a new one. This is effectively a new hash
 digest of the same length.
-}
combineHash :: Hash -> Hash -> Hash
combineHash h h' = hash (appendByteString h h')
{-# INLINEABLE combineHash #-}

checkHashLength :: Hash -> Bool
checkHashLength = (== 32) . lengthOfByteString
{-# INLINEABLE checkHashLength #-}

data MerkleTreeConfig = MerkleTreeConfig
  { mtcZeroRoot :: Hash -- zero root digest
  , mtcZeroLeaf :: Hash
  , mtcHeight :: Integer
  }
  deriving stock (Generic, Haskell.Show, Haskell.Eq)

PlutusTx.makeLift ''MerkleTreeConfig

PlutusTx.makeIsDataIndexed
  ''MerkleTreeConfig
  [('MerkleTreeConfig, 0)]

-- | A MerkleTree representation, suitable for on-chain manipulation.
data MerkleTree
  = MerkleEmpty
  | MerkleNode Hash MerkleTree MerkleTree
  | MerkleLeaf Hash
  deriving stock (Generic, Haskell.Show, Haskell.Eq)

PlutusTx.makeLift ''MerkleTree

PlutusTx.makeIsDataIndexed
  ''MerkleTree
  [('MerkleEmpty, 0), ('MerkleNode, 1), ('MerkleLeaf, 2)]

{- | Make empty Merkle Tree, filling leaves with MerkleEmpty
 and other nodes with zeros - list of zero digests starting from root, excluding zero leaf
-}
mkEmptyMT :: Integer -> Hash -> MerkleTree
mkEmptyMT height zeroLeaf = goCreate zeros
  where
    goCreate (h : hs) = MerkleNode h (goCreate hs) (goCreate hs)
    goCreate [] = MerkleEmpty
    zeros = reverse $ go height zeroLeaf
    go 0 _ = []
    go i h =
      let new = combineHash h h
       in new : go (i - 1) new
{-# INLINEABLE mkEmptyMT #-}

-- | Get zero root for tree of specific height and zero leaf
zeroRoot :: Integer -> Hash -> Hash
zeroRoot h zeroHash
  | h == 0 = zeroHash
  | otherwise =
      let new = combineHash zeroHash zeroHash
       in zeroRoot (h - 1) new
{-# INLINEABLE zeroRoot #-}

-- | Counter of leaves in a tree, should start from 0 for empty tree
type NextInsertionCounter = Integer

-- | From root to leaf, False - go left, True - go right
type MerkleProofPath = [Bool]

{- | Counter of next inserted item is converted to Merkle Path.
 A number in binary could be considered as path in a binary tree,
 e.g. for a tree of height 4: 8th insertion in binary is [True,False,False,False],
 which is interpreted as "to find where to insert new leaf go to [right,left,left,left]"
-}
counterToPath :: Integer -> NextInsertionCounter -> MerkleProofPath
counterToPath h n
  | n == 0 = zeroArr
  | n > 2 ^ h = traceError "Merkle tree is full"
  | otherwise = take (h - length binaryN) zeroArr <> binaryN
  where
    zeroArr = replicate h False
    binaryN = reverse $ go n
    go k
      | k == 0 = []
      | otherwise =
          let (d, m) = divMod k 2
           in if m == 0
                then False : go d
                else True : go d
{-# INLINEABLE counterToPath #-}

-- | Traverse a tree according to Merkle Path saving subtrees, which are complementary to the path
splitByPathMT :: MerkleProofPath -> MerkleTree -> [MerkleTree]
splitByPathMT path tree = snd $ foldl go (tree, []) path
  where
    go (MerkleNode _ l r, acc) p
      | p = (r, l : acc)
      | otherwise = (l, r : acc)
    go (t, acc) _ = (t, acc)
{-# INLINEABLE splitByPathMT #-}

{- | Starting from inserted leaf compose new Merkle Tree from Merkle Path subtrees,
 and rehash all path elements
-}
composeByPathMT :: Hash -> MerkleTree -> (Bool, MerkleTree) -> MerkleTree
composeByPathMT zeroLeaf l@(MerkleLeaf h) (_, MerkleEmpty) =
  MerkleNode (combineHash h zeroLeaf) l MerkleEmpty
composeByPathMT _ r@(MerkleLeaf hr) (_, l@(MerkleLeaf hl)) =
  MerkleNode (combineHash hl hr) l r
composeByPathMT _ acc@(MerkleNode hacc _ _) (p, el@(MerkleNode hel _ _))
  | p = MerkleNode (combineHash hel hacc) el acc
  | otherwise = MerkleNode (combineHash hacc hel) acc el
composeByPathMT _ _ _ = traceError "Not consistent Merkle Tree composition"
{-# INLINEABLE composeByPathMT #-}

{- | insert is done off-chain first, it returns new MerkleTree (it should be a part of contract state)
 it is then checked on-chain: newMerkleTree == insert depositedCommitment oldMerkleTree
-}
insert :: MerkleTreeConfig -> NextInsertionCounter -> Hash -> MerkleTree -> MerkleTree
insert MerkleTreeConfig {..} next commitment oldTree = foldl (composeByPathMT mtcZeroLeaf) newLeaf $ zip (reverse path) subTrees
  where
    path = counterToPath mtcHeight next
    subTrees = splitByPathMT path oldTree
    newLeaf = MerkleLeaf commitment
{-# INLINEABLE insert #-}

-- | Returns current root if Merkle Tree is not empty
currentRoot :: MerkleTreeConfig -> MerkleTree -> Maybe Hash
currentRoot MerkleTreeConfig {..} tree = case tree of
  MerkleNode root _ _
    | root == mtcZeroRoot -> Nothing
    | otherwise -> Just root
  _ -> Nothing
{-# INLINEABLE currentRoot #-}

nonEmptyLeafs :: MerkleTree -> [Hash]
nonEmptyLeafs (MerkleNode _ l r) = nonEmptyLeafs l <> nonEmptyLeafs r
nonEmptyLeafs (MerkleLeaf h) = [h]
nonEmptyLeafs MerkleEmpty = []
{-# INLINEABLE nonEmptyLeafs #-}

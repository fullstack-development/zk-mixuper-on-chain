-- | Implementation based on https://github.com/input-output-hk/hydra/tree/master/plutus-merkle-tree
module Service.MerkleTree where

import Ext.Plutarch.List (pdropI, preplicate)
import Ext.Plutarch.Num (ppow)
import Plutarch.DataRepr (HRec, PDataFields, PMemberFields)
import Plutarch.Extra.List (preverse)
import Plutarch.Extra.TermCont (pguardC, pletC)
import qualified Plutarch.Monadic as P
import Plutarch.Prelude

-- | A type for representing hash digests.
type PHash = PByteString

-- | Counter of leaves in a tree, should start from 0 for empty tree
type PNextInsertionCounter = PInteger

-- | From root to leaf, False - go left, True - go right
type PMerkleProofPath = PList PBool

newtype PMerkleTreeConfig (s :: S)
  = PMerkleTreeConfig (Term s (PDataRecord '["zeroRoot" := PHash, "zeroLeaf" := PHash, "height" := PInteger]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PDataFields, PIsData)

instance DerivePlutusType PMerkleTreeConfig where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PMerkleTreeConfig
instance PTryFrom PData (PAsData PMerkleTreeConfig)

data PMerkleTree (s :: S)
  = PMerkleEmpty (Term s (PDataRecord '[]))
  | PMerkleNode (Term s (PDataRecord '["value" := PHash, "leftSubtree" := PMerkleTree, "rightSubtree" := PMerkleTree]))
  | PMerkleLeaf (Term s (PDataRecord '["value" := PHash]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData)

instance DerivePlutusType PMerkleTree where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PMerkleTree
instance PTryFrom PData (PAsData PMerkleTree)

pmerkleEmpty :: Term s PMerkleTree
pmerkleEmpty = pcon $ PMerkleEmpty pdnil

pmerkleLeaf :: Term s PHash -> Term s PMerkleTree
pmerkleLeaf leaf =
  pcon $
    PMerkleLeaf $
      pdcons @"value" # pdata leaf # pdnil

pmerkleNode ::
  Term s PHash ->
  Term s PMerkleTree ->
  Term s PMerkleTree ->
  Term s PMerkleTree
pmerkleNode value l r =
  pcon $
    PMerkleNode $
      pdcons @"value"
        # pdata value
          #$ pdcons @"leftSubtree"
        # pdata l
          #$ pdcons @"rightSubtree"
        # pdata r
          #$ pdnil

newtype PMerkleTreeState (s :: S)
  = PMerkleTreeState (Term s (PDataRecord '["nextLeaf" := PNextInsertionCounter, "tree" := PMerkleTree]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PDataFields, PIsData)

instance DerivePlutusType PMerkleTreeState where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PMerkleTreeState
instance PTryFrom PData (PAsData PMerkleTreeState)

pmerkleTreeState :: Term s PNextInsertionCounter
  -> Term s PMerkleTree -> Term s PMerkleTreeState
pmerkleTreeState nextCounter tree =
  pcon $
    PMerkleTreeState $
      pdcons @"nextLeaf"
        # pdata nextCounter
          #$ pdcons @"tree"
        # pdata tree
          #$ pdnil

-- | Computes a SHA-256 hash of a given 'BuiltinByteString' message.
phash :: Term s (PByteString :--> PHash)
phash = psha2_256

{- | Combines two hashes digest into a new one. This is effectively a new hash
 digest of the same length.
-}
pcombineHash :: Term s (PHash :--> (PHash :--> PHash))
pcombineHash = phoistAcyclic $
  plam $
    \hl hr -> phash # (hl <> hr)

pcheckHashLength :: Term s (PByteString :--> PBool)
pcheckHashLength = phoistAcyclic $
  plam $
    \bs -> (plengthBS # bs) #== 32

-- | Returns current root if Merkle Tree is not empty
pcurrentRoot ::
  Term s (PMerkleTreeConfig :--> PMerkleTree :--> PMaybe PHash)
pcurrentRoot = phoistAcyclic $
  plam $
    \config tree -> pmatch tree \case
      PMerkleNode node ->
        let root = pfield @"value" # node
            zeroRoot = pfield @"zeroRoot" # config
         in pif (root #== zeroRoot) (pcon PNothing) (pcon $ PJust root)
      _ -> pcon PNothing

pnonEmptyLeafs ::
  Term s (PMerkleTree :--> PList PHash)
pnonEmptyLeafs =
  phoistAcyclic $
    pfix #$ plam \self tree -> pmatch tree \case
      PMerkleNode n -> P.do
        node <- pletFields @'["leftSubtree", "rightSubtree"] n
        pconcat # (self # node.leftSubtree) # (self # node.rightSubtree)
      PMerkleLeaf l ->
        let leaf = pfield @"value" # l
         in pcons # leaf # pnil
      PMerkleEmpty _ -> pnil

{- | Counter of next inserted item is converted to Merkle Path.
 A number in binary could be considered as path in a binary tree,
 e.g. for a tree of height 4: 8th insertion in binary is [True,False,False,False],
 which is interpreted as "to find where to insert new leaf go to [right,left,left,left]"
-}
pcounterToPath :: Term s (PInteger :--> PNextInsertionCounter :--> PMerkleProofPath)
pcounterToPath = phoistAcyclic $
  plam $
    \h n -> unTermCont do
      zeroArr <- pletC (preplicate # h # pconstant False)
      leavesSetCardinality <- pletC (ppow @PInteger # 2 # h)
      pguardC "Merkle tree is full" (n #<= leavesSetCardinality)
      pure $ pif (n #== 0) zeroArr P.do
        let binaryN = preverse #$ pcounterToPathImpl # n
        let prefixZeros = pdropI # (plength # binaryN) # zeroArr
        pconcat # prefixZeros # binaryN
  where
    pcounterToPathImpl ::
      Term s (PInteger :--> PMerkleProofPath)
    pcounterToPathImpl =
      phoistAcyclic $
        pfix #$ plam \self k -> pif (k #== 0) pnil P.do
          let d = pdiv # k # 2
          let m = pmod # k # 2
          pif
            (m #== 0)
            (pcons # pconstant False # (self # d))
            (pcons # pconstant True # (self # d))

-- | Traverse a tree according to Merkle Path saving subtrees, which are complementary to the path
psplitByPathMT :: Term s (PMerkleProofPath :--> PMerkleTree :--> PList PMerkleTree)
psplitByPathMT = phoistAcyclic $
  plam $
    \path tree -> P.do
      let res = pfoldl # reducer # pcon (PPair tree pnil) # path
      PPair _ mts <- pmatch res
      mts
  where
    reducer :: Term s (PPair PMerkleTree (PList PMerkleTree) :--> PBool :--> PPair PMerkleTree (PList PMerkleTree))
    reducer = phoistAcyclic $
      plam $
        \pair p -> P.do
          PPair t acc <- pmatch pair
          pmatch t \case
            PMerkleNode n -> P.do
              node <- pletFields @'["leftSubtree", "rightSubtree"] n
              pif
                p
                (pcon $ PPair node.rightSubtree (pcons # node.leftSubtree # acc))
                (pcon $ PPair node.leftSubtree (pcons # node.rightSubtree # acc))
            _ -> pcon $ PPair t acc

{- | Starting from inserted leaf compose new Merkle Tree from Merkle Path subtrees,
 and rehash all path elements
-}
pcomposeByPathMT :: Term s (PHash :--> PMerkleTree :--> PPair PBool PMerkleTree :--> PMerkleTree)
pcomposeByPathMT = phoistAcyclic $
  plam $
    \zeroLeaf tree pair -> P.do
      msg <- plet $ pconstant "Not consistent Merkle Tree composition"
      PPair p subtree <- pmatch pair
      pmatch tree \case
        PMerkleLeaf l -> P.do
          leaf <- plet (pfield @"value" # l)
          pmatch subtree \case
            PMerkleEmpty _ -> P.do
              -- tree inserted from left, empty subtree from right
              let value = pcombineHash # leaf # zeroLeaf
              pmerkleNode value tree pmerkleEmpty
            PMerkleLeaf subl -> P.do
              -- tree inserted from right, leaf subtree from left
              subleaf <- plet (pfield @"value" # subl)
              let value = pcombineHash # subleaf # leaf
              pmerkleNode value subtree tree
            _ -> ptraceError msg
        PMerkleNode n -> P.do
          node <- plet (pfield @"value" # n)
          pmatch subtree \case
            PMerkleNode subn -> P.do
              subnode <- plet (pfield @"value" # subn)
              pif
                p
                -- tree inserted from right, subtree from left
                (pmerkleNode (pcombineHash # subnode # node) subtree tree)
                -- tree inserted from left, subtree from right
                (pmerkleNode (pcombineHash # node # subnode) tree subtree)
            _ -> ptraceError msg
        _ -> ptraceError msg

{- | insert is done off-chain first, it returns new MerkleTree (it should be a part of contract state)
 it is then checked on-chain: newMerkleTree == insert depositedCommitment oldMerkleTree
-}
pinsertMT :: Term s (PMerkleTreeConfig :--> PHash :--> PMerkleTreeState :--> PMerkleTreeState)
pinsertMT = phoistAcyclic $
  plam $
    \config newLeaf is -> P.do
      conf <- pletFields @'["zeroLeaf", "height"] config
      inputState <- pletFields @'["nextLeaf", "tree"] is
      path <- plet $ pcounterToPath # conf.height # inputState.nextLeaf
      subtrees <- plet $ psplitByPathMT # path # inputState.tree
      let subtreeAndPaths = pzip # (preverse # path) # subtrees
      let outputTree = pfoldl # (pcomposeByPathMT # conf.zeroLeaf) # pmerkleLeaf newLeaf # subtreeAndPaths
      let outputCounter = inputState.nextLeaf + 1
      pmerkleTreeState outputCounter outputTree

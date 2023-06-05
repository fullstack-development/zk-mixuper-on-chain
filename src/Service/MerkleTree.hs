-- | Implementation based on https://github.com/input-output-hk/hydra/tree/master/plutus-merkle-tree
module Service.MerkleTree where

import Plutarch.DataRepr (HRec, PDataFields, PMemberFields)
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

newtype PMerkleTreeState (s :: S)
  = PMerkleTreeState (Term s (PDataRecord '["nextLeaf" := PNextInsertionCounter, "tree" := PMerkleTree]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PDataFields, PIsData)

instance DerivePlutusType PMerkleTreeState where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PMerkleTreeState
instance PTryFrom PData (PAsData PMerkleTreeState)

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

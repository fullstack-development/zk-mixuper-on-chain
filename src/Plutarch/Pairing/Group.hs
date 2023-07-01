module Plutarch.Pairing.Group (module Export, isOnCurve, pfrobeniusNaive, PG1, PG2, PGT, PG1Data, PG2Data, pG1fromDataTrusted, pG1fromData, pG2fromDataTrusted, pG2fromData) where

import Ext.Plutarch.Num (ppow)
import qualified Plutarch.Monadic as P
import Plutarch.Num (PNum (..))
import Plutarch.Pairing.BN128 (_q)
import Plutarch.Pairing.Group.Fq as Export
import Plutarch.Pairing.Group.Fq12 as Export
import Plutarch.Pairing.Group.Fq2 as Export
import Plutarch.Pairing.Group.Fq6 as Export
import Plutarch.Pairing.Group.Point as Export
import Plutarch.Prelude
import qualified Plutus.Pairing.BN128 as Plutus

type PG1 = PPoint PFq
type PG1Data = PBuiltinPair (PAsData PInteger) (PAsData PInteger)

type PFq2Data = PBuiltinPair (PAsData PInteger) (PAsData PInteger)

type PG2 = PPoint PFq2
type PG2Data = PBuiltinPair (PAsData PFq2Data) (PAsData PFq2Data)

type PGT = PFq12

pG1fromDataTrusted :: Term s PG1Data -> Term s PG1
pG1fromDataTrusted pt = pPoint (pfq $ pfromData $ pfstBuiltin # pt) (pfq $ pfromData $ psndBuiltin # pt)

pG1fromData :: Term s PG1Data -> Term s PG1
pG1fromData pt = pPoint (pfqSafe $ pfromData $ pfstBuiltin # pt) (pfqSafe $ pfromData $ psndBuiltin # pt)

pG2fromDataTrusted :: Term s PG2Data -> Term s PG2
pG2fromDataTrusted p = pPoint (pFq2 pxx pxy) (pFq2 pyx pyy)
  where
    px = pfromData $ pfstBuiltin # p
    py = pfromData $ psndBuiltin # p
    pxx = pfq $ pfromData $ pfstBuiltin # px
    pxy = pfq $ pfromData $ psndBuiltin # px
    pyx = pfq $ pfromData $ pfstBuiltin # py
    pyy = pfq $ pfromData $ psndBuiltin # py

pG2fromData :: Term s PG2Data -> Term s PG2
pG2fromData p = pPoint (pFq2 pxx pxy) (pFq2 pyx pyy)
  where
    px = pfromData $ pfstBuiltin # p
    py = pfromData $ psndBuiltin # p
    pxx = pfqSafe $ pfromData $ pfstBuiltin # px
    pxy = pfqSafe $ pfromData $ psndBuiltin # px
    pyx = pfqSafe $ pfromData $ pfstBuiltin # py
    pyy = pfqSafe $ pfromData $ psndBuiltin # py

-- | Test whether a value satisfies the corresponding curve equation
isOnCurve :: forall (s :: S) (a :: PType). (PIsData a, PNum a) => Term s (PPoint a) -> Term s PBool
isOnCurve pt = pmatch pt \case
  PInfinity _ -> pconstant True
  PPoint pr -> P.do
    p <- pletFields @'["x", "y"] pr
    let yy :: Term s a = p.y #* p.y
    let xxx :: Term s a = p.x #* p.x #* p.x
    let b :: Term s a = pfromInteger Plutus._b
    pdata yy #== pdata (xxx + b)

{- | Iterated frobenius morphisms on fields of characteristic _q,
 implemented naively
-}
pfrobeniusNaive :: (PNum a) => Term s (a :--> a)
pfrobeniusNaive = phoistAcyclic $ plam \a -> ppow # a # _q

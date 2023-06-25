module Plutarch.Pairing.Group (module Export, isOnCurve, pfrobeniusNaive, PG1, PG2, PGT) where

import Ext.Plutarch.Num (ppow)
import qualified Plutarch.Monadic as P
import Plutarch.Num (PNum (..))
import Plutarch.Pairing.BN128
import Plutarch.Pairing.Group.Fq as Export
import Plutarch.Pairing.Group.Fq12 as Export
import Plutarch.Pairing.Group.Fq2 as Export
import Plutarch.Pairing.Group.Fq6 as Export
import Plutarch.Pairing.Group.Point as Export
import Plutarch.Prelude
import qualified Plutus.Pairing.BN128 as Plutus

type PG1 = PPoint PFq

type PG2 = PPoint PFq2

type PGT = PFq12

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

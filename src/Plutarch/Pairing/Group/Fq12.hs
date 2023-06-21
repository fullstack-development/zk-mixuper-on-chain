module Plutarch.Pairing.Group.Fq12 where

import Ext.Plutarch.Num (ppow)
import Plutarch.DataRepr (PDataFields)
import qualified Plutarch.Monadic as P
import Plutarch.Num (PNum (..))
import Plutarch.Pairing.Group.Fq2 (pFq2, pfq2Conj)
import Plutarch.Pairing.Group.Fq6 (
  PFq6,
  pFq6,
  pfq6Inv,
  pmkFq6,
  pmulXiFq6,
 )
import Plutarch.Prelude

-- TODO
-- class PNormilize
-- pnormalize x = mapfields (fq `pmod` q) x

newtype PFq12 (s :: S)
  = PFq12 (Term s (PDataRecord '["x" := PFq6, "y" := PFq6]))
  deriving stock (Generic)
  deriving anyclass (PlutusType, PIsData, PDataFields, PEq, PShow)

instance DerivePlutusType PFq12 where
  type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PFq12
instance PTryFrom PData (PAsData PFq12)

instance PNum PFq12 where
  (#+) = pfq12Add
  (#-) = pfq12Sub
  (#*) = pfq12Mul
  pabs = phoistAcyclic $ plam id
  pnegate = phoistAcyclic $ plam (0 #-)
  psignum = plam \x ->
    pif
      (x #== 0)
      0
      1
  pfromInteger = pmkFq12

pFq12 :: Term s PFq6 -> Term s PFq6 -> Term s PFq12
pFq12 x y =
  pcon $
    PFq12 $
      pdcons @"x"
        # pdata x
          #$ pdcons @"y"
        # pdata y #$ pdnil

pmkFq12 :: Integer -> Term s PFq12
pmkFq12 i =
  pcon $
    PFq12 $
      pdcons @"x"
        # x
          #$ pdcons @"y"
        # y #$ pdnil
  where
    x = pdata (pmkFq6 i)
    y = pdata 0

pfq12Add :: Term s PFq12 -> Term s PFq12 -> Term s PFq12
pfq12Add at bt = P.do
  a <- pletFields @'["x", "y"] at
  b <- pletFields @'["x", "y"] bt
  pFq12 (a.x + b.x) (a.y + b.y)

pfq12Sub :: Term s PFq12 -> Term s PFq12 -> Term s PFq12
pfq12Sub at bt = P.do
  a <- pletFields @'["x", "y"] at
  b <- pletFields @'["x", "y"] bt
  pFq12 (a.x - b.x) (a.y - b.y)

pfq12Mul :: Term s PFq12 -> Term s PFq12 -> Term s PFq12
pfq12Mul at bt = P.do
  a <- pletFields @'["x", "y"] at
  b <- pletFields @'["x", "y"] bt
  xx <- plet (a.x #* b.x)
  yy <- plet (a.y #* b.y)
  pFq12 ((pmulXiFq6 # yy) + xx) ((a.x + a.y) * (b.x + b.y) - xx - yy)

-- | Multiplicative inverse
pfq12Inv :: Term s PFq12 -> Term s PFq12
pfq12Inv at = P.do
  a <- pletFields @'["x", "y"] at
  t <- plet $ pfq6Inv (a.x * a.x - (pmulXiFq6 # (a.y * a.y)))
  pFq12 (a.x * t) $ pnegate # (a.y * t)

-- | Conjugation
pfq12Conj :: Term s PFq12 -> Term s PFq12
pfq12Conj at = P.do
  a <- pletFields @'["x", "y"] at
  pFq12 a.x (pnegate # a.y)

pfastFrobenius1 :: Term s PFq12 -> Term s PFq12
pfastFrobenius1 term = P.do
  arg <- pletFields @'["x", "y"] term
  a <- pletFields @'["x", "y", "z"] arg.x
  b <- pletFields @'["x", "y", "z"] arg.y
  cax <- plet $ pfq2Conj a.x
  cbx <- plet $ pfq2Conj b.x
  cay <- plet $ pfq2Conj a.y
  cby <- plet $ pfq2Conj b.y
  caz <- plet $ pfq2Conj a.z
  cbz <- plet $ pfq2Conj b.z
  x <- plet $ pFq6 cax (cay * gamma2) (caz * gamma4)
  y <- plet $ pFq6 (cbx * gamma1) (cby * gamma3) (cbz * gamma5)
  pFq12 x y
  where
    gamma1 = pFq2 8376118865763821496583973867626364092589906065868298776909617916018768340080 16469823323077808223889137241176536799009286646108169935659301613961712198316
    gamma2 = pFq2 21575463638280843010398324269430826099269044274347216827212613867836435027261 10307601595873709700152284273816112264069230130616436755625194854815875713954
    gamma3 = pFq2 2821565182194536844548159561693502659359617185244120367078079554186484126554 3505843767911556378687030309984248845540243509899259641013678093033130930403
    gamma4 = pFq2 2581911344467009335267311115468803099551665605076196740867805258568234346338 19937756971775647987995932169929341994314640652964949448313374472400716661030
    gamma5 = pFq2 685108087231508774477564247770172212460312782337200605669322048753928464687 8447204650696766136447902020341177575205426561248465145919723016860428151883

{- | Unitary exponentiation @^@.

 Exponentiation of a unitary element @x@ to an arbitrary integer @n@
 in a specified cyclotomic subgroup.
-}
ppowUnitary :: Term s PFq12 -> Term s PInteger -> Term s PFq12
ppowUnitary x n =
  pif
    (n #< 0)
    (ppow # pfq12Conj x # negate n)
    (ppow # x # n)

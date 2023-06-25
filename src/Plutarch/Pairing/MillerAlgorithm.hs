module Plutarch.Pairing.MillerAlgorithm (ppairing) where

import Control.Monad (join)
import qualified Plutarch.Monadic as P
import Plutarch.Num (PNum (..))
import Plutarch.Pairing.BN128 (parameterBin, parameterHex)
import Plutarch.Pairing.Group (
  PFq12,
  PFq2,
  PG1,
  PG2,
  PGT,
  PPoint (..),
  pFq12,
  pFq2,
  pFq6,
  pInfinity,
  pPoint,
  pfastFrobenius1,
  pfq12Conj,
  pfrobeniusNaive,
  ppowUnitary,
 )
import Plutarch.Prelude
import qualified PlutusTx.Monoid as PlutusTx
import qualified PlutusTx.Semigroup as PlutusTx

ppairing :: Term s (PG1 :--> PG2 :--> PGT)
ppairing =
  phoistAcyclic $
    plam \p q ->
      pfinalExponentiationBN
        # parameterHex
          #$ pmillerAlgorithmBN
        # parameterBin
        # p
        # q

pmillerAlgorithmBN :: Term s (PList PInteger :--> PG1 :--> PG2 :--> PGT)
pmillerAlgorithmBN =
  phoistAcyclic $
    plam \list p q -> pmatch list \case
      PSNil -> 1
      PSCons x xs -> P.do
        q' <- plet $ pif (x #<= 0) (PlutusTx.inv q) q
        pfinalStepBN
          # p
          # q
            #$ pmillerLoop
          # p
          # q
          # xs
          # pcon (PPair q' 1)

pfinalStepBN :: Term s (PG1 :--> PG2 :--> PPair PG2 PGT :--> PGT)
pfinalStepBN =
  phoistAcyclic $
    plam \p q pair -> P.do
      PPair t f <- pmatch pair
      q1 <- plet $ pfrobTwisted # q
      q2 <- plet $ PlutusTx.inv $ pfrobTwisted # q1
      PPair t' f' <- pmatch $ plineFunction # p # t # q1
      PPair _ f'' <- pmatch $ plineFunction # p # t' # q2
      f * f' * f''

pmillerLoop :: Term s (PG1 :--> PG2 :--> PList PInteger :--> PPair PG2 PGT :--> PPair PG2 PGT)
pmillerLoop =
  phoistAcyclic $
    pfix #$ plam \self p q list pair -> pmatch list \case
      PSNil -> pair
      PSCons x xs -> P.do
        next <- plet $ pdoublingStep # p # pair
        pif (x #== 0) (self # p # q # xs # next) $
          pif
            (x #== 1)
            (self # p # q # xs #$ padditionStep # p # q # next)
            (self # p # q # xs #$ padditionStep # p # PlutusTx.inv q # next)

pdoublingStep :: Term s (PG1 :--> PPair PG2 PGT :--> PPair PG2 PGT)
pdoublingStep =
  phoistAcyclic $
    plam \p pair -> P.do
      PPair t f <- pmatch pair
      PPair tq l <- pmatch $ plineFunction # p # t # t
      pcon $ PPair tq $ l * f * f

padditionStep :: Term s (PG1 :--> PG2 :--> PPair PG2 PGT :--> PPair PG2 PGT)
padditionStep =
  phoistAcyclic $
    plam \p q pair -> P.do
      PPair t f <- pmatch pair
      PPair tq l <- pmatch $ plineFunction # p # q # t
      pcon $ PPair tq $ l * f

pfinalExponentiationBN :: Term s (PInteger :--> PGT :--> PGT)
pfinalExponentiationBN =
  phoistAcyclic $
    plam \u -> hardPart u . easyPart
  where
    easyPart :: Term s PGT -> Term s PGT
    easyPart =
      let
        p6 f = pfq12Conj f * PlutusTx.inv f
        p2 f = f * (pfastFrobenius1 . pfastFrobenius1) f
       in
        p2 . p6

    hardPart :: Term s PInteger -> Term s PGT -> Term s PGT
    hardPart u f = P.do
      fu <- plet $ ppowUnitary f u
      fu2 <- plet $ ppowUnitary fu u
      fu3 <- plet $ ppowUnitary fu2 u
      fpu <- plet $ pfastFrobenius1 fu2
      y0 <- plet $ pfastFrobenius1 (f * pfastFrobenius1 (f * pfastFrobenius1 f))
      y1 <- plet $ pfq12Conj f
      y2 <- plet $ pfastFrobenius1 fpu
      y3 <- plet $ pfq12Conj $ pfastFrobenius1 fu
      y4 <- plet $ pfq12Conj $ fu * fpu
      y5 <- plet $ pfq12Conj fu2
      y6 <- plet $ pfq12Conj $ fu3 * pfastFrobenius1 fu3
      p4'' <- plet $ y4 * y5 * join (*) y6
      p4' <- plet $ join (*) $ p4'' * y2 * join (*) (p4'' * y3 * y5)
      p4' * y0 * join (*) (p4' * y1)

plineFunction :: Term s (PG1 :--> PG2 :--> PG2 :--> PPair PG2 PGT)
plineFunction =
  phoistAcyclic $
    plam \pt tt qt -> pmatch pt \case
      PInfinity _ -> inf
      PPoint pr -> pmatch tt \case
        PInfinity _ -> inf
        PPoint tr -> pmatch qt \case
          PInfinity _ -> inf
          PPoint qr -> P.do
            p <- pletFields @'["x", "y"] pr
            t <- pletFields @'["x", "y"] tr
            q <- pletFields @'["x", "y"] qr
            yy :: Term s PFq2 <- plet $ t.y + q.y
            l <- plet $ (q.y - t.y) * PlutusTx.inv (q.x - t.x)
            x1 <- plet $ l * l - t.x - q.x
            y1 <- plet $ l * (t.x - x1) - t.y
            xx <- plet $ t.x * t.x
            l' <- plet $ (xx + xx + xx) * PlutusTx.inv (t.y + t.y)
            x2 <- plet $ l' * l' - t.x - q.x
            y2 <- plet $ l' * (t.x - x2) - t.y
            let case0 = pFq12 (pFq6 (pFq2 p.x 0) 0 0) (pFq6 (pnegate # t.x) 0 0)
            let case1 = pFq12 (pFq6 (pFq2 (pnegate # p.y) 0) 0 0) (pFq6 (pFq2 p.x 0 * l) (t.y - l * t.x) 0)
            let case2 = pFq12 (pFq6 (pFq2 (pnegate # p.y) 0) 0 0) (pFq6 (pFq2 p.x 0 * l') (t.y - l' * t.x) 0)
            pif (pdata yy #== pdata 0) (pcon $ PPair pInfinity case0) $
              pif
                (pnot #$ t.x #== q.x)
                (pcon $ PPair (pPoint x1 y1) case1)
                (pcon $ PPair (pPoint x2 y2) case2)
  where
    inf :: Term s (PPair (PPoint a) PFq12)
    inf = pcon $ PPair pInfinity 1

pfrobTwisted :: Term s (PG2 :--> PG2)
pfrobTwisted =
  phoistAcyclic $
    plam \pt -> pmatch pt \case
      PInfinity _ -> pInfinity
      PPoint pr -> P.do
        p <- pletFields @'["x", "y"] pr
        pPoint ((pfrobeniusNaive # p.x) * xitx) ((pfrobeniusNaive # p.y) * xity)
  where
    xitx =
      pFq2
        21575463638280843010398324269430826099269044274347216827212613867836435027261
        10307601595873709700152284273816112264069230130616436755625194854815875713954

    xity =
      pFq2
        2821565182194536844548159561693502659359617185244120367078079554186484126554
        3505843767911556378687030309984248845540243509899259641013678093033130930403

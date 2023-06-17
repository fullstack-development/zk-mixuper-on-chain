{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

-- | Implementation of the optimal Ate pairing on the curve BN128
module Plutus.Pairing.MillerAlgorithm where

import Ext.PlutusTx.Monad
import Ext.PlutusTx.Numeric
import Plutus.Pairing.BN128
import Plutus.Pairing.Group
import Plutus.Pairing.Group.Fq as Fq
import Plutus.Pairing.Group.Fq12 as Fq12
import Plutus.Pairing.Group.Fq2 as Fq2
import Plutus.Pairing.Group.Fq6 as Fq6
import Plutus.Pairing.Group.Point
import PlutusTx.Prelude
import qualified Prelude as Haskell

{-# INLINEABLE pairing #-}
pairing :: G1 -> G2 -> GT
pairing p1 p2 =
  finalExponentiationBN parameterHex $
    millerAlgorithmBN parameterBin p1 p2

-------------------------------------------------------------------------------
-- Miller algorithm
-------------------------------------------------------------------------------

{- | [Miller algorithm for Barreto-Naehrig curves]
 (https://eprint.iacr.org/2010/354.pdf).
-}
millerAlgorithmBN :: [Integer] -> G1 -> G2 -> GT
millerAlgorithmBN (x : xs) p q =
  finalStepBN p q $
    millerLoop p q xs (if x > 0 then q else inv q, mempty)
millerAlgorithmBN _ _ _ = mempty
{-# INLINEABLE millerAlgorithmBN #-}

-- Final step for BN curves, line 11 to line 13.
finalStepBN :: G1 -> G2 -> (G2, GT) -> GT
finalStepBN p q (t, f) = case lineFunction p t q1 of
  (t', f') -> case lineFunction p t' q2 of
    (_, f'') -> f <> f' <> f''
  where
    q1 = frobTwisted q
    q2 = inv $ frobTwisted q1
{-# INLINEABLE finalStepBN #-}

-- Miller loop, line 2 to line 10.
millerLoop :: G1 -> G2 -> [Integer] -> (G2, GT) -> (G2, GT)
millerLoop p q = millerLoop'
  where
    millerLoop' [] tf = tf
    millerLoop' (x : xs) tf = case doublingStep p tf of
      tf2
        | x == 0 -> millerLoop' xs tf2
        | x == 1 -> millerLoop' xs $ additionStep p q tf2
        | otherwise -> millerLoop' xs $ additionStep p (inv q) tf2
{-# INLINEABLE millerLoop #-}

-- Doubling step, line 4.
doublingStep :: G1 -> (G2, GT) -> (G2, GT)
doublingStep p (t, f) = (<>) f . (<>) f <$> lineFunction p t t
{-# INLINEABLE doublingStep #-}

-- Addition step, line 6 and line 8.
additionStep :: G1 -> G2 -> (G2, GT) -> (G2, GT)
additionStep p q (t, f) = (<>) f <$> lineFunction p q t
{-# INLINEABLE additionStep #-}

-------------------------------------------------------------------------------
-- Final exponentiation
-------------------------------------------------------------------------------

{- | [Final exponentiation for Barreto-Naehrig curves]
 (https://eprint.iacr.org/2010/354.pdf).
-}
finalExponentiationBN :: Integer -> GT -> GT
finalExponentiationBN u = hardPart . easyPart
  where
    easyPart = p2 . p6
      where
        p6 f = fq12Conj f * inv f -- f^(p^6 - 1)
        p2 f = f * (fastFrobenius1 . fastFrobenius1) f -- f^(p^2 + 1)
    hardPart f = p4
      where
        fu = powUnitary f u -- f^u
        fu2 = powUnitary fu u -- f^(u^2)
        fu3 = powUnitary fu2 u -- f^(u^3)
        fpu = fastFrobenius1 fu2 -- f^(pu^2)
        y0 = fastFrobenius1 (f * fastFrobenius1 (f * fastFrobenius1 f)) -- f^(p + p^2 + p^3)
        y1 = fq12Conj f -- f^(-1)
        y2 = fastFrobenius1 fpu -- f^(p^2u^2)
        y3 = fq12Conj $ fastFrobenius1 fu -- f^(-pu)
        y4 = fq12Conj $ fu * fpu -- f^(-u - pu^2)
        y5 = fq12Conj fu2 -- f^(-u^2)
        y6 = fq12Conj $ fu3 * fastFrobenius1 fu3 -- f^(-u^3 - pu^3)
        p4 = p4' * y0 * join (*) (p4' * y1) -- f^((p^4 - p^2 + 1) / r)
          where
            p4' = join (*) $ p4'' * y2 * join (*) (p4'' * y3 * y5)
            p4'' = y4 * y5 * join (*) y6
{-# INLINEABLE finalExponentiationBN #-}

-------------------------------------------------------------------------------
-- Auxiliary functions
-------------------------------------------------------------------------------

{- | Line function evaluation @Line(T, Q, P)@.

 Compute the line function between two points @T@ and @Q@ in @G2@,
 evaluate the line function at a point @P@ in @G1@,
 and embed the line function evaluation in @GT@.
-}
lineFunction ::
  -- | Point @P@.
  G1 ->
  -- | Point @T@.
  G2 ->
  -- | Point @Q@.
  G2 ->
  -- | Points @T + Q@ and @Line(T, Q, P)@.
  (G2, GT)
lineFunction (Point x y) (Point x1 y1) (Point x2 y2)
  | x1 /= x2 = (Point x3 y3, Fq12 (Fq6 (Fq2 (zero - y) zero) zero zero) (Fq6 (Fq2 x zero * l) (y1 - l * x1) zero))
  | y1 + y2 == zero = (Infinity, Fq12 (Fq6 (Fq2 x zero) zero zero) (Fq6 (zero - x1) zero zero))
  | otherwise = (Point x3' y3', Fq12 (Fq6 (Fq2 (zero - y) zero) zero zero) (Fq6 (Fq2 x zero * l') (y1 - l' * x1) zero))
  where
    l = (y2 - y1) * inv (x2 - x1)
    x3 = l * l - x1 - x2
    y3 = l * (x1 - x3) - y1
    x12 = x1 * x1
    l' = (x12 + x12 + x12) * inv (y1 + y1)
    x3' = l' * l' - x1 - x2
    y3' = l' * (x1 - x3') - y1
lineFunction _ _ _ = (Infinity, mempty)
{-# INLINEABLE lineFunction #-}

{- | Twisted Frobenius endomorphism @Frob(P)@.

 Compute the Frobenius endomorphism on a point @P@ given a twist @xi@.
-}
frobTwisted ::
  -- | Point @P@.
  G2 ->
  -- | Point @Frob(P)@.
  G2
frobTwisted (Point x y) = Point (frobeniusNaive x * xitx) (frobeniusNaive y * xity)
  where
    -- tx = quotient (_q - 1) 3
    -- tx = 7296080957279758407415468581752425029565437052432607887563012631548408736194
    -- ty = shiftR _q 1
    -- ty = 10944121435919637611123202872628637544348155578648911831344518947322613104291
    -- xitx = pow xi tx
    xitx :: Fq2
    xitx =
      Fq2
        (Fq 21575463638280843010398324269430826099269044274347216827212613867836435027261)
        (Fq 10307601595873709700152284273816112264069230130616436755625194854815875713954)
    -- xity = pow xi ty
    xity :: Fq2
    xity =
      Fq2
        (Fq 2821565182194536844548159561693502659359617185244120367078079554186484126554)
        (Fq 3505843767911556378687030309984248845540243509899259641013678093033130930403)
frobTwisted _ = Infinity
{-# INLINEABLE frobTwisted #-}

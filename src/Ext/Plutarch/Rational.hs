module Ext.Plutarch.Rational where

import qualified Plutarch.Monadic as P
import Plutarch.Prelude

type GCD = PPair PInteger (PPair PInteger PInteger)

{- | Extended Euclidean algorithm.
 Given non-negative a and b, return x, y and g
 such that ax + by = g, where g = gcd(a,b).
 Note that x or y may be negative.
 Can be used to compute inverse as if gcd(a,b) == 1 then ax == 1 mod b
-}
pgcdExt :: Term s (PInteger :--> PInteger :--> GCD)
pgcdExt =
  phoistAcyclic $
    pfix #$ plam \self a b ->
      pif (b #== 0) (pcon $ PPair 1 (pcon $ PPair 0 a)) P.do
        q <- plet $ pquot # a # b
        r <- plet $ prem # a # b
        PPair s pair <- pmatch $ self # b # r
        PPair t g <- pmatch pair
        pcon $ PPair t (pcon $ PPair (s - q * t) g)

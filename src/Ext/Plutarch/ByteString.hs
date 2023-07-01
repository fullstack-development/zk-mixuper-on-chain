module Ext.Plutarch.ByteString where

import Ext.Plutarch.List (pdropI, preplicate)
import Plutarch.Extra.List (preverse)
import qualified Plutarch.Monadic as P
import Plutarch.Prelude

-- | Convert ByteString of arbitrary size to integer
pbyteString2Integer :: Term s (PInteger :--> PByteString :--> PInteger)
pbyteString2Integer = phoistAcyclic $
  plam $
    \size bs -> pbytes2Num #$ pmap # (pindexBS # bs) # (indexFrom # size # 0)
  where
    indexFrom :: Term s (PInteger :--> PInteger :--> PList PInteger)
    indexFrom =
      phoistAcyclic $
        pfix #$ plam \self size i ->
          pif (i #== size) pnil $
            pcons # i #$ self # size # (i + 1)

-- | Convert bytes to number
pbytes2Num :: Term s (PList PInteger :--> PInteger)
pbytes2Num = phoistAcyclic $
  plam $
    \bytes -> pbits2Num #$ pfoldl # pconcat # pnil #$ pmap # pbyte2Bits # bytes

{- | Convert bits to number
 uses little-endian system (stores the least-significant bit at the smallest address)
-}
pbits2Num :: Term s (PList PBool :--> PInteger)
pbits2Num = phoistAcyclic $
  plam $
    \bits -> P.do
      PPair res _ <- pmatch $ pfoldl # add # pcon (PPair 0 1) # bits
      res
  where
    add :: Term s (PPair PInteger PInteger :--> PBool :--> PPair PInteger PInteger)
    add = phoistAcyclic $
      plam $
        \pair bit -> P.do
          PPair num powOf2 <- pmatch pair
          pcon $ PPair (toI bit * powOf2 + num) (2 * powOf2)

    toI :: Term s PBool -> Term s PInteger
    toI b = pif b 1 0

-- | Convert byte sized integer to bits
pbyte2Bits :: Term s (PInteger :--> PList PBool)
pbyte2Bits = phoistAcyclic $
  plam $
    \n -> P.do
      zeroArr <- plet (preplicate # 8 # pconstant False)
      pif (n #== 0) zeroArr P.do
        let binaryN = preverse #$ go # n
        let prefixZeros = pdropI # (plength # binaryN) # zeroArr
        pconcat # prefixZeros # binaryN
  where
    go ::
      Term s (PInteger :--> PList PBool)
    go =
      phoistAcyclic $
        pfix #$ plam \self k -> pif (k #== 0) pnil P.do
          let d = pdiv # k # 2
          let m = pmod # k # 2
          pif
            (m #== 0)
            (pcons # pconstant False # (self # d))
            (pcons # pconstant True # (self # d))

-- stack repl Withdraw.hs --package cryptonite --package bytestring --package memory

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Crypto.Hash as H
import qualified Crypto.Random as R
import qualified Data.Bits as Bit
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.Foldable
import Data.Word

{-
Consider a tree of height 2 corresponding to circuit Withdraw(2):
        root
    /           \
h(l,zeroLeaf) zero[1]
  /     \
 l   zeroLeaf
Lets say that our commitment is l.
Then merkle path is:
[zeroLeaf,zero[1]]
[0,0]
-}

recipientPubKeyHash :: BS.ByteString
recipientPubKeyHash = ".\n\214\f2\a$\140\236\212}\189\227\215R\224\170\209A\214\184\248\SUB\194\198\236\162|"

relayerPubKeyHash :: BS.ByteString
relayerPubKeyHash = "\162\194\fw\136z\206\FS\217\134\EM>Nu\186\189\137\147\207\213i\149\205\\\252\230\t\194"

root :: BS.ByteString
root =
  let h = convert $ H.hashWith H.SHA256 $ l <> zeroLeaf
   in convert $ H.hashWith H.SHA256 $ h <> zero1

zero1 :: BS.ByteString
zero1 = convert $ H.hashWith H.SHA256 $ zeroLeaf <> zeroLeaf

zeroLeaf :: BS.ByteString
zeroLeaf = convert $ H.hashWith H.SHA256 ("" :: BS.ByteString)

lNullifier :: BS.ByteString
lNullifier = "\177|\243\189\179\&4\206.x\141\189\191\DC2y\225ET\162\174\&1\186\FS\210+\208\176\169:$1I"

lSecret :: BS.ByteString
lSecret = "\ETB\STX\245\175\255\251Y\194@8\b\199,z\NAKXm\141#\137^Kc\204d\241\DC1\218\&7oK"

rNullifier :: BS.ByteString
rNullifier = ">n\232\DC2K\209QW\b|\248m\188\209\130\b\138\207\179\153\209\&7\"\232\v\"h\SOHP^\ETB"

rSecret :: BS.ByteString
rSecret = "1~\189\\\ENQ*=\187\240[\fa\162\148\196\238%\220\&8\163\\\140\170\170\SOHv\163\246&\240\151"

l :: BS.ByteString
l = convert $ H.hashWith H.SHA256 $ lNullifier <> lSecret

lNullifierHash :: BS.ByteString
lNullifierHash = convert $ H.hashWith H.SHA256 lNullifier

r :: BS.ByteString
r = convert $ H.hashWith H.SHA256 $ rNullifier <> rSecret

rNullifierHash :: BS.ByteString
rNullifierHash = convert $ H.hashWith H.SHA256 rNullifier

h :: BS.ByteString
h = convert $ H.hashWith H.SHA256 $ l <> r

h' :: BS.ByteString
h' = "\151\193\199\232Q\146\246\DEL\134v\228\178g\ETX\232I\221\SI\r\128t\133\&1\249\214\a\139\157\237\163]\181"

-- root :: BS.ByteString
-- root = convert $ H.hashWith H.SHA256 $ h <> h'

-- 248-bit entropy
genByteString :: IO BS.ByteString
genByteString = R.getRandomBytes 31

-- 256-bit entropy
genByteStringHash :: IO BS.ByteString
genByteStringHash = R.getRandomBytes 32

bits2Num :: [Bool] -> Integer
bits2Num = fst . foldl add (0, 1)
  where
    add (!num, !powOf2) bit = (toI bit * powOf2 + num, 2 * powOf2)
    toI b = if b then 1 else 0

toBits :: BS.ByteString -> [Bool]
toBits = foldMap unpackWord . BS.unpack

fromBits :: [Bool] -> BS.ByteString
fromBits = BS.pack . fmap packWord . unw
  where
    unw words = case splitAt 8 words of
      ([], []) -> []
      (pre, []) -> [pre]
      (pre, rest) -> pre : unw rest

unpackWord :: Word8 -> [Bool]
unpackWord w = foldl' f [] [0 .. 7]
  where
    f acc i = Bit.testBit w i : acc

packWord :: [Bool] -> Word8
packWord bs = fromInteger $ foldr f 0 $ zip bs $ reverse [0 .. 7]
  where
    f (b, i) acc =
      if b
        then acc + 2 ^ i
        else acc

convert :: H.Digest a -> BS.ByteString
convert = BA.convert

module MyLib where

import Plutarch.Bool (pif')
import Plutarch.Prelude

someFunc :: IO ()
someFunc = putStrLn "someFunc"

pfun :: Term s (a :--> b)
pfun = undefined

hfun :: Term s a -> Term s b
hfun x = pfun # x

true :: Term s PBool
true = pconstant True

trueData :: Term s (PAsData PBool)
trueData = pconstantData True

x :: Term s PInteger
x = pconstant 42

justX :: Term s (PMaybe PInteger)
justX = pcon $ PJust x

origin :: Term s PInteger
origin = 0

hello :: Term s PString
hello = "Hello!"

-- | A plutarch level bytestring. Its value is [65], in this case.
hexs :: Term s PByteString
hexs = phexByteStr "41"
-- ^ 'phexByteStr' interprets a hex string as a bytestring. 0x41 is 65 - of course.

pid :: Term s (a :--> a)
pid = plam id

matcher :: PMaybe a s -> Term s PBool
matcher = \case
  PJust _ -> true
  PNothing -> pconstant False

hisJust :: Term s (PMaybe a) -> Term s PBool
hisJust x = pmatch x matcher

pisJust :: Term s (PMaybe a :--> PBool)
pisJust = plam hisJust

hif :: Term s PBool -> Term s a -> Term s a -> Term s a
hif cond onTrue onFalse = pforce $ pif' # cond # pdelay onTrue # pdelay onFalse

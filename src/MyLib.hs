module MyLib where

import Plutarch.Prelude

someFunc :: IO ()
someFunc = putStrLn "someFunc"

pfun :: Term s (a :--> b)
pfun = undefined

hfun :: Term s a -> Term s b
hfun x = pfun # x

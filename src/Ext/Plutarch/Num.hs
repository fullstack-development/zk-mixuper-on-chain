module Ext.Plutarch.Num where

import Plutarch.Extra.TermCont (pguardC)
import Plutarch.Num
import Plutarch.Prelude

ppow :: (PNum a) => Term s (a :--> PInteger :--> a)
ppow = phoistAcyclic $
  plam $
    \x0 y0 -> unTermCont do
      pguardC "Negative exponent" (0 #<= y0)
      pure $ pif (y0 #== 0) 1 (f # x0 # y0)
  where
    f :: (PNum a) => Term s (a :--> PInteger :--> a)
    f =
      phoistAcyclic $
        pfix #$ plam \self x y ->
          pif (peven # y) (self # (x #* x) # (pquot # y # 2)) $
            pif (y #== 1) x $
              g # (x #* x) # (pquot # y # 2) # x
    g :: (PNum a) => Term s (a :--> PInteger :--> a :--> a)
    g =
      phoistAcyclic $
        pfix #$ plam \self x y z ->
          pif (peven # y) (self # (x #* x) # (pquot # y # 2) # z) $
            pif (y #== 1) (x #* z) $
              self # (x #* x) # (pquot # y # 2) # (x #* z)

peven :: Term s (PInteger :--> PBool)
peven = phoistAcyclic $ plam $ \n -> pmod # n # 2 #== 0

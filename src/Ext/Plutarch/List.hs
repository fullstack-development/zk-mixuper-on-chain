module Ext.Plutarch.List where

import Plutarch.Prelude

preplicate ::
  Term s (PInteger :--> a :--> PList a)
preplicate =
  phoistAcyclic $
    pfix #$ plam \self n a ->
      pif (n #<= 0) pnil (pcons # a # (self # (n - 1) # a))

pdropI ::
  Term s (PInteger :--> PList a :--> PList a)
pdropI =
  phoistAcyclic $
    pfix #$ plam \self n xxs -> pif (n #<= 0) xxs P.do
      let f _ xs = self # (n - 1) # xs
      pelimList f pnil xxs

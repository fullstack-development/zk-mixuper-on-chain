module Plutarch.Pairing.Group.Class where

import Plutarch.Prelude

class PSemigroup (a :: PType) where
  pappend :: Term s a -> Term s a -> Term s a

class (PSemigroup a) => PMonoid (a :: PType) where
  pidentity :: Term s a

class (PMonoid a) => PGroup (a :: PType) where
  pinv :: Term s a -> Term s a

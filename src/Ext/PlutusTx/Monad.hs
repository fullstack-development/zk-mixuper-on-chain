{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}

module Ext.PlutusTx.Monad where

-- | Monadic join specialized for ((->) r)
join :: (a -> a -> b) -> a -> b
join op r = r `op` r
{-# INLINEABLE join #-}

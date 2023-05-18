{-# LANGUAGE ImpredicativeTypes #-}

module Fib where

import Data.Default (def)
import Data.Function (fix)
import Data.Text (Text)
import Plutarch (compile, prettyTerm)
import Plutarch.Evaluate (EvalError, evalTerm)
import Plutarch.Prelude
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget)
import PlutusCore.Pretty (Doc)

ifThenElse :: Bool -> a -> a -> a
ifThenElse b x y = if b then x else y

hfib' :: Integer -> Integer
hfib' 0 = 0
hfib' 1 = 1
hfib' n = hfib' (n - 1) + hfib' (n - 2)

hfib :: Integer -> Integer
hfib = fix \self n ->
  ifThenElse (n == 0) 0 $
    ifThenElse (n == 1) 1 $
      self (n - 1) + self (n - 2)

pfib ::
  Term
    s
    (PInteger :--> PInteger)
pfib =
  phoistAcyclic $
    pfix #$ plam \self n ->
      pif (n #== 0) 0 $
        pif (n #== 1) 1 (self # (n - 1) + self # (n - 2))

evalT ::
  ClosedTerm a ->
  Either
    Text
    ( Either EvalError (Doc ())
    , ExBudget
    , [Text]
    )
evalT = fmap (\(res, budget, log) -> (prettyTerm def <$> res, budget, log)) . evalTerm def

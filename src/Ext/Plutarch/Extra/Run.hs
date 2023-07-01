module Ext.Plutarch.Extra.Run where

import Data.Bifunctor (Bifunctor (..))
import Data.ByteString.Short (ShortByteString)
import Data.Default (Default (..))
import Data.Text (Text, pack)
import Plutarch (ClosedTerm, Script, compile)
import Plutarch.Evaluate (evalScript')
import Plutarch.Prelude
import Plutarch.Script (Script (..), serialiseScript)
import Plutonomy (optimizeUPLC)
import PlutusCore (
  DeBruijn,
  DefaultFun,
  DefaultUni,
 )
import PlutusCore.Data (Data)
import PlutusCore.Evaluation.Machine.ExBudget (ExBudget (ExBudget))
import PlutusCore.Evaluation.Machine.ExMemory (ExCPU (..), ExMemory (..))
import PlutusCore.MkPlc (mkConstant, mkIterApp)
import PlutusPrelude (over)
import UntypedPlutusCore (Program, progTerm)

applyArguments :: Script -> [Data] -> Script
applyArguments (Script p) args =
  let termArgs = mkConstant () <$> args
      applied t = mkIterApp () t termArgs
   in Script $ over progTerm applied p

evalSerialize :: ClosedTerm a -> Either Text ShortByteString
evalSerialize x = serialiseScript . (\(a, _, _) -> a) <$> evalT x

evalT :: ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT x = evalWithArgsT x []

evalWithArgsT :: ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT x args = do
  cmp <- compile def x
  let optimized = Script $ optimizeUPLC (unScript cmp)
  let budget = ExBudget (ExCPU 10_000_000_000) (ExMemory 14_000_000)
  let (escr, budg, trc) = evalScript' budget $ applyArguments optimized args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)

evalWithArgsT' :: ClosedTerm a -> [Data] -> Either Text (Program DeBruijn DefaultUni DefaultFun (), ExBudget, [Text])
evalWithArgsT' x args =
  (\(res, budg, trcs) -> (unScript res, budg, trcs))
    <$> evalWithArgsT x args

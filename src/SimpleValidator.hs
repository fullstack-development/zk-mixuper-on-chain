module SimpleValidator where

import Plutarch.Api.V1 (PRedeemer)
import Plutarch.Api.V2
import qualified Plutarch.Monadic as P
import Plutarch.Prelude

simpleValidator ::
  Term
    s
    ( PAsData PPubKeyHash
        :--> PAsData PDatum
        :--> PAsData PRedeemer
        :--> PAsData PScriptContext
        :--> PUnit
    )
simpleValidator = plam $ \pkh _datm _redm ctx' -> P.do
  ctx <- pletFields @["txInfo", "purpose"] ctx'
  PSpending _ <- pmatch ctx.purpose
  let signatories = pfield @"signatories" # ctx.txInfo
  pif
    (pelem # pkh # pfromData signatories)
    (pconstant ())
    perror

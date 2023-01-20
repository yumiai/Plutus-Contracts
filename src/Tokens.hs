{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Tokens where

import Data.Aeson
import GHC.Generics
import PlutusTx
import Ledger
import Data.Void
import Text.Printf
import Plutus.Contract
import Ledger.Constraints
import qualified Data.Map                       as Map
import qualified Ledger.Constraints             as Constraints
import qualified Ledger.Typed.Scripts           as Scripts
-- import qualified Plutus.Contract.Trace          as Trace
import qualified Control.Monad.Freer.Extras     as Extras
import qualified Plutus.Trace.Emulator                    as Emulator
import qualified Prelude                        as Haskell
import Ledger.Value                             as Value
import Plutus.V1.Ledger.Scripts                 as V1
import Plutus.Script.Utils.V1.Scripts           as Utils
import PlutusTx.Builtins                        as Builtins
import Ledger.Ada                               as Ada
import Schema                                   (ToSchema)
import Prelude                                  (Semigroup (..))
import PlutusTx.Prelude                         hiding (Monoid (..), Semigroup (..))
import Data.Text
import Wallet.Emulator
import qualified Ledger.Constraints as Scripts

{- [ Mint & Burn! ] 
create a contract that can mint and burn
tokens.
-}

newtype TokenRedeemer = TokenRedeemer PaymentPubKeyHash

PlutusTx.unstableMakeIsData ''TokenRedeemer

-- {-# INLINABLE mkTokenValidator #-}

mkTokenValidator :: TokenRedeemer -> ScriptContext -> Bool
mkTokenValidator (TokenRedeemer pkh) ScriptContext{scriptContextTxInfo=txinfo} =
    traceIfFalse "transaction not signed by given key" checkSigned
    where
        checkSigned = txSignedBy txinfo $ unPaymentPubKeyHash pkh

policy :: MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedMintingPolicy mkTokenValidator

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy

-- off chain code.

data MintParams = MintParams
    {_TokenName :: !TokenName
    ,_Ammount   :: !Integer
    }
    deriving (ToSchema,FromJSON,Generic,ToJSON, Haskell.Show)

type MintScheme = Endpoint "mint" MintParams

mintMe :: forall w s e. AsContractError e => MintParams -> Contract w s e ()
mintMe MintParams{..} = do
    pkh <- ownFirstPaymentPubKeyHash
    let
        val = Value.singleton curSymbol _TokenName _Ammount
        lookups = Scripts.mintingPolicy policy
        tx = mustMintValueWithRedeemer (Redeemer $ toBuiltinData $ TokenRedeemer pkh) val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    _        <- awaitTxConfirmed $ getCardanoTxId ledgerTx        
    logInfo @Text $ pack $ printf "tx Confirmed minted: %s " (Haskell.show _TokenName)
    let
        outputs = getCardanoTxOutputs ledgerTx
        getVal tx = maybe Haskell.mempty _ciTxOutValue (fromTxOut tx)
        foldedVals = foldMap getVal outputs
        adaBal = valueOf foldedVals adaSymbol adaToken
        tokenBal = valueOf foldedVals curSymbol _TokenName
    logInfo @Text $ pack $ printf "your Ada Balance is: %s " (Haskell.show adaBal)
    logInfo @Text $ pack $ printf "your %s Balance is: %s " (Haskell.show _TokenName) (Haskell.show tokenBal)


mintyEndpoints :: Contract () MintScheme Text ()
mintyEndpoints = awaitPromise (endpoint @"mint" mintMe) >> mintyEndpoints

runTrace :: Haskell.IO ()
runTrace = Emulator.runEmulatorTraceIO tokenTrace

tokenTrace :: Emulator.EmulatorTrace ()
tokenTrace = do
    let
        w1 = knownWallet 1
    h1 <- Emulator.activateContractWallet w1 mintyEndpoints
    Emulator.callEndpoint @"mint" h1 (MintParams "ABC" 20)
    _ <- Emulator.waitNSlots 5
    Emulator.callEndpoint @"mint" h1 (MintParams "ABC" (-10))
    _ <- Emulator.waitNSlots 3
    return ()
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

module FortyTwo where

import PlutusTx
import Ledger
import Text.Printf
import Plutus.Contract
import Ledger.Constraints
import qualified Data.Map                       as Map 
import qualified Ledger.Constraints             as Constraints
import qualified Ledger.Typed.Scripts           as Scripts
import Plutus.Trace.Emulator                    as Emulator
import qualified Prelude                        as Haskell
import Plutus.V1.Ledger.Scripts                 as V1
import PlutusTx.Builtins                        as Builtins
import Ledger.Ada                               as Ada
import Prelude                                  (Semigroup (..))
import PlutusTx.Prelude                         hiding (Monoid (..), Semigroup (..))
import Data.Text
import Wallet.Emulator


{- [ The meaning of life challenge ]
Write a contract that sends ada to a script. The script should make sure 
that anyone grabing a transaction that the redeemer is equal to 42 
otherwise the resulting transaction will fail. This contract was done in
on of the ealier examples in the plutus-pioneer-program week02.
-}

newtype MeaningOfLife =  MeaningOfLife Integer 
                         deriving stock (Haskell.Show,Haskell.Eq)

PlutusTx.unstableMakeIsData ''MeaningOfLife


data MeaningOfLifeType
instance Scripts.ValidatorTypes MeaningOfLifeType where
    type DatumType    MeaningOfLifeType = ()
    type RedeemerType MeaningOfLifeType = MeaningOfLife

{-# INLINABLE whatIsTheMeaningOfLife #-}

whatIsTheMeaningOfLife :: () -> MeaningOfLife -> ScriptContext -> Bool
whatIsTheMeaningOfLife ()  (MeaningOfLife int) _ = traceIfFalse "this is not the meaning of life" checkNumber
    where
        checkNumber :: Bool
        checkNumber = int == 42

typedValidator  :: Scripts.TypedValidator MeaningOfLifeType
typedValidator = Scripts.mkTypedValidator @MeaningOfLifeType
    $$(PlutusTx.compile [|| whatIsTheMeaningOfLife ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator @() @MeaningOfLife


scriptValidator :: Validator
scriptValidator = Scripts.validatorScript typedValidator

validatorHash :: ValidatorHash
validatorHash = Scripts.validatorHash typedValidator

validatorAddr :: Address
validatorAddr = Scripts.validatorAddress typedValidator


-- Off Chain Code

-- newtype LifeParams = LifeParams Integer
--     deriving stock (Haskell.Show,Haskell.Eq,Generic)
--     deriving anyclass (ToSchema,FromJSON,ToJSON)

type LifeEndpoints = Endpoint "send" Integer .\/
                     Endpoint "grab" Integer

send :: AsContractError e => Integer -> Contract w s e ()
send int = do
    let
        tx = Constraints.mustPayToTheScript () $ Ada.lovelaceValueOf int
    ledgerTx <- submitTxConstraints typedValidator tx
    _        <- awaitTxConfirmed $ getCardanoTxId ledgerTx 
    logInfo @Haskell.String $ printf "transaction submitted"
    return ()

grab :: forall w s e. AsContractError e => Integer -> Contract w s e ()
grab molf = do
    utxos <- utxosAt validatorAddr
    let
        orefs = Map.keys utxos                  
        createConstraints r = Constraints.mustSpendScriptOutput r (Redeemer $ toBuiltinData $ MeaningOfLife molf)
        
        lookups :: ScriptLookups MeaningOfLifeType
        lookups = Constraints.otherScript scriptValidator  <> Constraints.unspentOutputs utxos

        tx :: TxConstraints (Scripts.RedeemerType MeaningOfLifeType) (Scripts.DatumType MeaningOfLifeType)
        tx = foldMap createConstraints orefs
    case molf == 42 of
        True -> do
            ledgerTx <- submitTxConstraintsWith lookups tx 
            _        <- awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @Text $ pack "meaning of life confirmed"
        False -> logError @Text $ pack $ printf "this is not the meaning of life: %d" molf


lifeEndpoints :: Contract () LifeEndpoints Text ()
lifeEndpoints = selectList [send', grab'] >> lifeEndpoints
    where
        send' = endpoint @"send" send 
        grab' = endpoint @"grab" grab

runSim :: Haskell.IO ()
runSim = runEmulatorTraceIO lifeTrace

lifeTrace :: EmulatorTrace ()
lifeTrace = do
    let
        w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 lifeEndpoints
    h2 <- activateContractWallet w2 lifeEndpoints
    callEndpoint @"send" h1 20_000_000
    _ <- Emulator.waitNSlots 5
    callEndpoint @"grab" h2 52
    _ <- Emulator.waitNSlots 5
    callEndpoint @"grab" h2 42
    _ <- Emulator.waitNSlots 5
    return ()

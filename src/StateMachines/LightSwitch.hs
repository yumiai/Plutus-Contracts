{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE NumericUnderscores     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module StateMachines.LightSwitch where

import PlutusTx
import Data.Aeson
import Data.Monoid (Last(..))
import Ledger
import Data.Void
import Data.Functor (void)
import GHC.Generics (Generic)
import Playground.Contract (ToSchema)
import Data.Text
import Text.Printf
import Plutus.Contract                          as Contract
import qualified Control.Monad.Freer.Extras     as E
import Ledger.Constraints
import qualified Ledger.Constraints             as Constraints
import qualified Ledger.Typed.Scripts           as Scripts
import Plutus.Trace.Emulator                    as Emulator hiding (throwError)
import Wallet.Emulator
import qualified Prelude                        as HP
import Plutus.V1.Ledger.Scripts                 as V1
import PlutusTx.Builtins                        as Builtins
import Ledger.Ada                               as Ada
import Prelude                                  (Semigroup (..))
import PlutusTx.Prelude                         hiding (Monoid (..), Semigroup (..))
import Plutus.Contract.StateMachine

{- [ Light Switch State Machine **DEPRECATED**]
There are 2 possible transitions at play here

When the light switch is on, A transaction is made to pay someone
when the light switch is off, the state of the machine is finished 

-}

data Parameters = Parameters {_Recipient :: !PaymentPubKeyHash
                            , _Ammount   :: !Integer
                            } deriving HP.Show
PlutusTx.makeLift ''Parameters

data Switch = On | Off
    deriving (HP.Show,HP.Eq, Generic, FromJSON, ToJSON)

instance Eq Switch where
    {-# INLINABLE (==) #-}
    (==) On  On  = True
    (==) Off Off = True
    (==) _   _   = False

PlutusTx.unstableMakeIsData ''Switch

newtype SwitchDatum = SwitchDatum (Maybe Switch) 
    deriving (HP.Show,HP.Eq)

instance Eq SwitchDatum where
    {-# INLINABLE (==) #-}
    SwitchDatum (Just On)   == SwitchDatum (Just On)   = True
    SwitchDatum  (Just Off) ==  SwitchDatum (Just Off) = True
    _                       ==  _                      = False
 
PlutusTx.unstableMakeIsData ''SwitchDatum

{-# INLINABLE transition #-}
transition :: Parameters -> State SwitchDatum -> () -> Maybe (TxConstraints Void Void, State SwitchDatum)
transition Parameters {..} State{..} _ = case (stateData,()) of
    (SwitchDatum (Just s), ()) | s == On -> Just (Constraints.mustPayToPubKey _Recipient (lovelaceValueOf _Ammount),State (SwitchDatum $ Just s) HP.mempty)
    (SwitchDatum (Just s), ()) | s == Off -> Nothing
    (SwitchDatum Nothing, ())             -> Nothing
    _                           -> Nothing

{- Note [State Machine Redeemer..]
Well the state machine client works the only problem is that I dont have an input.
so the state machine has no way of updating the state.
it also only has one constaint.

note [State Machine Values]
the machines seems to create multiples scripts of itself when taking multiple endpoint calls
will need to re-write the contract tomorrow.
-}


{-# INLINABLE finalState #-}
finalState :: SwitchDatum -> Bool
finalState (SwitchDatum Nothing) = True
finalState _ = False

{-# INLINABLE switchStateMachine #-}
switchStateMachine :: Parameters -> StateMachine SwitchDatum ()
switchStateMachine p = StateMachine
    { smTransition = transition p
    , smFinal      = finalState
    , smCheck      = (\_ _ _ -> True)
    , smThreadToken = Nothing 
    }

{-# INLINABLE mkSwitchValidator #-}
mkSwitchValidator :: Parameters -> SwitchDatum -> () -> ScriptContext -> Bool
mkSwitchValidator p  = mkValidator $ switchStateMachine p

type SwitchWrap = StateMachine SwitchDatum ()

typedSwitchValidator :: Parameters -> Scripts.TypedValidator SwitchWrap
typedSwitchValidator p = Scripts.mkTypedValidator @SwitchWrap
    ($$(PlutusTx.compile [|| mkSwitchValidator ||])
    `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator @SwitchDatum @()

switchValidator :: Parameters -> Validator
switchValidator = Scripts.validatorScript . typedSwitchValidator

switchAddr :: Parameters -> Address
switchAddr = scriptAddress . switchValidator

switchClient :: Parameters -> StateMachineClient SwitchDatum ()
switchClient p = mkStateMachineClient $ StateMachineInstance (switchStateMachine p) (typedSwitchValidator p)

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . HP.show

data FirstParams = Params' {     _recipient   :: !PaymentPubKeyHash
                               , _ammount     :: !Integer
                               , _switchState :: !Switch
                               } deriving (HP.Show,FromJSON,ToJSON,Generic)

type SwitchScehma = Endpoint "flickSwitch" FirstParams

pullTheLever :: forall s w. FirstParams -> Contract w s Text ()
pullTheLever Params'{..} = do
    let
        params = Parameters _recipient _ammount
        client = switchClient params
        v      = lovelaceValueOf _ammount
        c      = _switchState
    _ <- mapError' $ runInitialise client (SwitchDatum (Just On)) v
    _ <- Contract.waitNSlots 3
    m <- mapError' $ getOnChainState  client
    case m of
        Nothing -> Contract.throwError "no games found"
        Just (o, _) -> case getStateData o of
            SwitchDatum (Just s) | s == Off -> do
                logInfo @HP.String "found a lightswitch..."
                _ <- mapError' $ runStep client ()
                logInfo @HP.String "But Everything was dark"
            SwitchDatum (Just s) | s == On -> do
                logInfo @HP.String "found a lightswitch..."
                _ <- mapError' $ runStep client ()
                logInfo @HP.String "the room is now has light"
            SwitchDatum Nothing -> do
                logInfo @HP.String "found a lightswitch..."
                logInfo @HP.String "yet it appears to be broken?"

switchEndpoints :: Contract () SwitchScehma Text ()
switchEndpoints = awaitPromise send' >> switchEndpoints
    where
        send' = endpoint @"flickSwitch" pullTheLever


runSim :: HP.IO ()
runSim = runEmulatorTraceIO myTrace

myTrace :: EmulatorTrace ()
myTrace = do
    let
        w1 = knownWallet 1
        w2 = knownWallet 2
        fp = Params' (mockWalletPaymentPubKeyHash w2)  20000000 On
        fp2 = Params' (mockWalletPaymentPubKeyHash w1) 20_000_000 Off
    h1 <- activateContractWallet w1 switchEndpoints
    h2 <- activateContractWallet w2 switchEndpoints
    callEndpoint @"flickSwitch" h1 fp
    _ <- Emulator.waitNSlots 3
    callEndpoint @"flickSwitch" h2 fp2
    return ()

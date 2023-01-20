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

module StateMachines.LightSwitch2 where

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


{- [ Light Switch State Machine 2]
There are 2 possible transitions at play here

When the light switch is on, A transaction is made to pay someone
when the light switch is off, the state of the machine is finished 

last version didnt have an input so the transition didnt really work.
the custom datum type was also a maybe so it could fail. so the redeemer should
just take the switch type and have constructor that tells it to finish.

  [see other note..]
-}
{- [ Flawed LightSwitch model ] 
Back to the drawing board with this one, I got a little bit more of an understanding
with the inputs of the state but the problem is that I need to create another contract
but at this point I might as well start again. I'll have to write out a diagram, it's 
too difficult to think about transitions and code at the same time.

time to ask GPT3 for a little help with an example as this one is proving a bit flawed.
-}

data Parameters = Parameters {_recipient :: !PaymentPubKeyHash
                            , _ammount   :: !Integer
                            } deriving HP.Show

PlutusTx.makeLift ''Parameters

data Switch = On | Off
    deriving (HP.Show,HP.Eq, Generic, FromJSON, ToJSON)

instance Eq Switch where
    On  == On   = True
    Off == Off = True
    _   == _   = False

PlutusTx.unstableMakeIsData ''Switch

data SwitchDatum = SwitchDatum (Maybe Switch) | Finished
    deriving (HP.Show,HP.Eq, Generic, FromJSON, ToJSON)

instance Eq SwitchDatum where
    {-# INLINABLE (==) #-}
    SwitchDatum s == SwitchDatum s' = s == s'
    Finished      == Finished       = True
    _             == _              = False

PlutusTx.unstableMakeIsData ''SwitchDatum

data SwitchInput = SwitchInput Switch
        deriving (HP.Show,HP.Eq, Generic, FromJSON, ToJSON)

instance Eq SwitchInput where
    {-# INLINABLE (==) #-}
    SwitchInput On   == SwitchInput On   = True
    SwitchInput Off  == SwitchInput Off  = True
    _           ==  _                          = False

PlutusTx.unstableMakeIsData ''SwitchInput

{-# INLINABLE lovelaces #-}
lovelaces :: Ledger.Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE transition #-}
transition :: Parameters -> State SwitchDatum -> SwitchInput -> Maybe (TxConstraints Void Void, State SwitchDatum)
transition Parameters {..} State{..} r = case (stateValue,stateData, r) of
    (_,SwitchDatum Nothing, SwitchInput s)               -> Just ( HP.mempty,State (SwitchDatum $ Just s) HP.mempty )

    (_,SwitchDatum (Just s), SwitchInput s') 
        | s' == s && s' == On -> Just ( Constraints.mustPayToPubKey _recipient $ lovelaceValueOf _ammount
                                                               , State (SwitchDatum $ Just s') $ lovelaceValueOf _ammount )
    (_,SwitchDatum (Just s), SwitchInput s') 
        | s' == s && s' == Off                           -> Just ( HP.mempty,State Finished HP.mempty )
    
    
    _                                                    -> Nothing


{-# INLINABLE final #-}
final :: SwitchDatum -> Bool
final Finished = True
final _        = False

{-# INLINABLE switchStateMachine #-}

switchStateMachine :: Parameters -> StateMachine SwitchDatum SwitchInput
switchStateMachine p = StateMachine
    { smTransition  = transition p
    , smFinal       = final
    , smCheck       = (\_ _ _ -> True)
    , smThreadToken = Nothing
    }

{-# INLINABLE mkSwitchValidator #-}
mkSwitchValidator :: Parameters -> SwitchDatum -> SwitchInput -> ScriptContext -> Bool
mkSwitchValidator p  = mkValidator $ switchStateMachine p

type SwitchWrap = StateMachine SwitchDatum SwitchInput

typedSwitchValidator :: Parameters -> Scripts.TypedValidator SwitchWrap
typedSwitchValidator p = Scripts.mkTypedValidator @SwitchWrap
    ($$(PlutusTx.compile [|| mkSwitchValidator ||])
    `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator @SwitchDatum @SwitchInput

switchValidator :: Parameters -> Validator
switchValidator = Scripts.validatorScript . typedSwitchValidator

switchAddr :: Parameters -> Address
switchAddr = scriptAddress . switchValidator

switchClient :: Parameters -> StateMachineClient SwitchDatum SwitchInput
switchClient p = mkStateMachineClient $ StateMachineInstance (switchStateMachine p) (typedSwitchValidator p)

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . HP.show

data FirstParams = Params' {     recipient   :: !PaymentPubKeyHash
                               , ammount     :: !Integer
                               , switchState :: !Switch
                               } deriving (HP.Show,FromJSON,ToJSON,Generic)

type SwitchScehma = Endpoint "flickSwitch" FirstParams


pullTheLever :: forall s w. FirstParams -> Contract w s Text ()
pullTheLever Params'{..} = do
    let
        params = Parameters recipient ammount
        client = switchClient params
        v      = lovelaceValueOf ammount
        c      = switchState
    _ <- mapError' $ runInitialise client (SwitchDatum Nothing) v
    _ <- Contract.waitNSlots 3
    m <- mapError' $ getOnChainState  client
    case m of
        Nothing -> Contract.throwError "no games found"
        Just (o, _) -> case getStateData o of
            SwitchDatum Nothing -> do
                 logInfo @HP.String $ printf "I found a lightswitch... I got Nothing"
                 _ <- mapError' $ runStep client (SwitchInput c)
                 logInfo @HP.String $ printf "state updated %s" (HP.show c)
            SwitchDatum (Just s) | s == c -> do
                 logInfo @HP.String $ printf "I found a lightswitch...%s" (HP.show s)
                 _ <- mapError' $ runStep client (SwitchInput c)
                 logInfo @HP.String $ printf "state updated %s" (HP.show c)
            _                   -> throwError "unexpected datum"

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
        fp = Params' (mockWalletPaymentPubKeyHash w2)  45_000_000 On
        fp2 = Params' (mockWalletPaymentPubKeyHash w2) 45_000_000 Off
        fp3 = Params' (mockWalletPaymentPubKeyHash w2) 45_000_000 Off
        -- fp2 = Params' (mockWalletPaymentPubKeyHash w1) 20_000_000 Off
    h1 <- activateContractWallet w1 switchEndpoints
    -- h2 <- activateContractWallet w2 switchEndpoints
    callEndpoint @"flickSwitch" h1 fp
    _ <- Emulator.waitNSlots 5
    return ()

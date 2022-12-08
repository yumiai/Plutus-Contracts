{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TokenMint1.SendToScriptMint where

import Control.Monad
import Data.Aeson
import Data.Map as Map
import Data.Text                    as T
import Data.Void
import GHC.Generics 
import Plutus.Contract
import qualified PlutusTx
import Ledger hiding (singleton)
import Ledger.Constraints
import Ledger.Tx
import PlutusTx.Builtins              as Builtins
import Plutus.V1.Ledger.Scripts       as V1
import Plutus.Script.Utils.V1.Scripts as Utils
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Ada                     as Ada
import Ledger.Value                   as Value
import Playground.Contract
import Playground.TH
import Playground.Types
import Wallet.Emulator
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Prelude (IO, Semigroup (..), Show (..), String, undefined)
import Text.Printf
import Data.ByteString                    as ByteString
import Playground.Contract
import Data.Maybe                  (fromJust)
import Plutus.Contract.Wallet      (getUnspentOutput)
import Data.Either (fromLeft,fromRight)


-- {-# INLINABLE sendToScriptMint #-}
-- I want this contract to send tokens to a script and allow wallets to grab the tokens 
-- the validator and minting policy should be thick as bricks and always succeed. This contract
-- is only to explore the possibility of spending,producing and minting tokens to wallets and scripts.

type SandAsset = AssetClass

defToken :: TokenName
defToken = tokenName "SAND"

defAssetClass :: SandAsset
defAssetClass = assetClass curSymbol defToken

minLoveLace :: Ledger.Value
minLoveLace = Ada.lovelaceValueOf 2_000_000

sendToScriptMint :: BuiltinData -> ScriptContext  -> Bool
sendToScriptMint _ _ = True

policy :: MintingPolicy
policy = mkMintingPolicyScript
    $$(PlutusTx.compile [|| Scripts.mkUntypedMintingPolicy sendToScriptMint ||])

policyHash :: MintingPolicyHash
policyHash = mintingPolicyHash policy

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy


{-# INLINABLE mkSillyValidator #-}
-- the second most low level a validator can get
-- this always succeeds it takes no logic to pass validation
-- I only created this dumb validator in order to create something to send tokens

-- mkSillyValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
-- mkSillyValidator _ _ _ = ()

mkSillyValidator :: () -> () -> ScriptContext -> Bool
mkSillyValidator _ _ _ = True

data SillyType

instance Scripts.ValidatorTypes SillyType where
    type instance DatumType SillyType = ()
    type instance RedeemerType SillyType = ()

sillyValidatorPlutus :: Scripts.TypedValidator SillyType
sillyValidatorPlutus = Scripts.mkTypedValidator @SillyType 
        $$(PlutusTx.compile [|| mkSillyValidator ||])
        $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator @() @()

-- lowLevelValidatorPlutus :: Validator
-- lowLevelValidatorPlutus = mkValidatorScript $$(PlutusTx.compile [|| mkSillyValidator ||])


sillyValidator :: Validator
sillyValidator = Scripts.validatorScript sillyValidatorPlutus

valHash :: ValidatorHash
valHash = validatorHash sillyValidator

scrAddress :: Address
scrAddress = scriptAddress sillyValidator

-- =-=-=-=-=-=-= 
-- Off-Chain Code
-- =-=-=-=-=-=-= 

data MintingParams = MintingParams 
            { _tokenName :: !TokenName,
              _tokenAmmount :: !Integer
            } deriving (Show,ToJSON,FromJSON,Generic,ToSchema)

type MintSchema = Endpoint "give" MintingParams .\/ 
                  Endpoint "grab" ()

give :: MintingParams -> Contract w s Text ()
-- perhaps using runError for the type :: Contract w s e (Either e a) for better error processing.
give MintingParams{..} = do
        let
            val = Value.singleton curSymbol _tokenName _tokenAmmount
            -- this value must only contain custom tokens to fit in with the minting contraint
            lookups = mintingPolicy policy
                -- without this the token will fail to mint, the minting policy is needed in the lookups in order to construct the transaction
            constraints :: TxConstraints Void Void
            constraints = mustMintValue val <> mustPayToOtherScript valHash unitDatum (val <> minLoveLace)
                {- Note: [ On Constraints...]
                Beacuse I forgot that outputs needed lovelace in the transaction it failed calling insuffcient funds. 
                Make sure  outputs have minimum 2 ADA using mustPayToTheScript constraint fails to combine with other 
                constraints other than itself because of an ambiguous redeemer or datum type variables.
                -}
        checkTokenName _tokenName -- make sure asset matches SAND
        ledgerTx <- mkTxConstraints @Void lookups constraints
        logInfo @String $ printf "created transaction with constraints: %s" (show ledgerTx)
        balTx <- balanceTx ledgerTx
        logDebug @String $ printf "transaction Balanced: %s" (show balTx)
        subTx <- submitBalancedTx balTx
        _ <- awaitTxConfirmed $ getCardanoTxId subTx
        logInfo @String $ printf "submitted transaction with val: %s" (show val)
        return ()

-- an individual HOF for checking token name in the offchain code.
checkTokenName ::  TokenName -> Contract w s Text ()
checkTokenName x 
        | x == defToken = return ()
        | otherwise = throwError . T.pack $ printf "TokenName must match %s! User defined _tokenName = %s" (toString defToken) (toString x)

{- Note: [ Rewrite ze grab function ]
would it be prudent to create two grab functions? 
one for only grabbing utxo's with asset class y?
I find using x - 100 annoying. Surely theres a better
way of resctring grabs to only 100 tokens rather than 
using arithmatic.
 -}

grab :: Contract w s Text ()
grab = do
    pkh <- ownFirstPaymentPubKeyHash
    utxos <- utxosAt scrAddress
    let
        orefs = Map.keys utxos
        lookups = Constraints.unspentOutputs utxos <>
                  Constraints.otherScript sillyValidator
        
        tx :: TxConstraints Void Void
        tx = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs]

        spendTx :: Map TxOutRef ChainIndexTxOut -> Contract w s Text ()
        spendTx utxo
            | Map.null utxo = throwError . T.pack $ printf "no utxos at script address"
            | otherwise = do
                ledgerTx <- submitTxConstraintsWith @Void lookups tx
                _ <- awaitTxConfirmed $ getCardanoTxId ledgerTx
                logInfo @String $ printf "%s sucessfully grabbed utxos" (show pkh)
                return ()
    spendTx utxos

{- | Base Grab Function
currently spends all the unspent outputs at a given script address.
The current condition is as it stands depending on which wallet calls 
the contract, he who calls the contract shall recieve thy the funds.
-}

endpoints :: Contract () MintSchema Text ()
endpoints = selectList [give', grab'] >> endpoints
    where
        give' = endpoint @"give" give
        grab' = endpoint @"grab" $ const grab


{- [ THE FUNCTION GRAVEYARD ]
He who finds these dead functions thus hath find them useful.. 
may they bless thy and rest in peace.  
-}

-- grab :: Contract w s Text ()
-- grab = do
--     pkh <- ownFirstPaymentPubKeyHash
--     utxos <- utxosAt scrAddress
--     tkn <- findValue
--     case Map.null utxos of
--         True -> logInfo @String $ printf "no utxos at address: %s" (show scrAddress) 
--         False  -> do
--             let
--                 orefs = Map.keys utxos
--                 tknBal = Value.valueOf getBal curSymbol defToken
--                 -- maybe find the val in the DecoratedTxOut and reduce by 100
--                 -- what happens when balance is 0?
                
--                 getBal = case isRight tkn of
--                     True -> (\(Right x) -> x ) tkn
--                     _ -> minLoveLace
                
--                 val = Value.singleton curSymbol defToken (tknBal - 100) <> minLoveLace
--                 lookups :: ScriptLookups SillyType
--                 lookups = Constraints.unspentOutputs utxos                       <>
--                           Constraints.otherScript sillyValidator                 
--                         --   Constraints.typedValidatorLookups sillyValidatorPlutus
--                 tx = case tkn of
--                     Right _ -> mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
--                                        Constraints.mustPayToOtherScript valHash unitDatum val <>
--                                        Constraints.mustPayToPubKey pkh minLoveLace
                    
--                     Left _ -> mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
--                               mustPayToPubKey pkh minLoveLace
                
--             ledgerTx <- submitTxConstraintsWith lookups tx
--             _ <- awaitTxConfirmed $ getCardanoTxId ledgerTx
--             logInfo @String $ printf "submitted transaction"
--             return ()

-- catchError :: Contract () s e ()
-- catchError = handleError (\err -> logError err ) grab


-- findValue :: Contract w s e0 (Either Text Value.Value)
-- findValue = runError $ do
--     utxos <- utxosAt scrAddress
--     let
--         txVal = [ (oref,o) | (oref,o) <- Map.toList utxos ]
--         utxoCheck x
--             | Map.null x = throwError . T.pack $ printf "no utxos"
--             | otherwise = case txVal of
--                     [(_,o)] -> return $ mconcat [_ciTxOutValue o]
--                     _          -> throwError . T.pack $ printf "failed to find TxOutValue at output"
--     utxoCheck utxos
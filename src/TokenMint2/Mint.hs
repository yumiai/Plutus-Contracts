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
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module TokenMint2.Mint where

import           Control.Monad
import           Data.Aeson
import qualified Data.Semigroup                 as Semigroup
import           Data.Text                      as T
import           Data.Void                      (Void)
import           GHC.Generics
import           Ledger                         hiding (singleton)
import           Ledger.Constraints
import qualified Ledger.Constraints             as Constraints
import qualified Ledger.Typed.Scripts           as Scripts
import           Ledger.Value                   as Value
import           Plutus.Contract                as Contract
import           Plutus.Contract.Wallet         (getUnspentOutput)
import           Plutus.Script.Utils.V1.Scripts as Utils
import           Plutus.V1.Ledger.Scripts       as V1
import qualified PlutusTx
import qualified PlutusTx.AssocMap              as AssocMap
import           PlutusTx.Builtins              as Builtins
import           PlutusTx.Prelude               hiding (Monoid (..),
                                                 Semigroup (..))
import           Prelude                        (Semigroup (..))
import qualified Prelude                        as Haskell
import           Schema                         (ToSchema)
import           Text.Printf

data Recipient = RecipientParams
  { _refOutput   :: (TxId, Integer),
    _tokenParams :: AssocMap.Map TokenName Integer
  }
  deriving stock (Haskell.Eq, Haskell.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''Recipient

mkRecipient :: TxOutRef -> (TokenName, Integer) -> Recipient
mkRecipient (TxOutRef h i) (tn, ammount) =
  RecipientParams
    { _refOutput = (h, i),
      _tokenParams = AssocMap.singleton tn ammount
    }

mkRecipientValue :: CurrencySymbol -> Recipient -> Value.Value
mkRecipientValue s RecipientParams {_tokenParams = amnts} = PlutusTx.Prelude.fold values
  where
    values = PlutusTx.Prelude.fmap (uncurry $ Value.singleton s) (AssocMap.toList amnts)

mkPolicy :: Recipient -> () -> ScriptContext -> Bool
mkPolicy rcp@RecipientParams {_refOutput = (ref, indx)} () ctx@ScriptContext {scriptContextTxInfo = txInfo} =
  traceIfFalse "Value minted different from what was expected" checkMinted
    && traceIfFalse "Pending transaction failed spend the designated output" checkOref
  where
    scriptCurSymbol :: CurrencySymbol
    scriptCurSymbol = ownCurrencySymbol ctx

    checkMinted :: Bool
    checkMinted = expected == current
      where
        expected :: Value.Value
        expected = mkRecipientValue scriptCurSymbol rcp

        current :: Value.Value
        current = txInfoMint txInfo

    checkOref :: Bool
    checkOref = spendsOutput txInfo ref indx

curVali :: Recipient -> MintingPolicy

policy rcp =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.mkUntypedMintingPolicy . mkPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode rcp

policyHash :: Recipient -> MintingPolicyHash
policyHash = mintingPolicyHash . policy

curSymbol :: Recipient -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

mkValidator :: Recipient -> Validator
mkValidator = Validator . unMintingPolicyScript . policy

{- [ Off-Chain Code ] -}
data RecipientP = RecipientP
  { _asset      :: !(TokenName, Integer),
    _recipients :: ![PaymentPubKeyHash]
  }
  deriving stock (Generic, Haskell.Show)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

type RecipientSchema = Endpoint "mint" RecipientP

mintContract :: forall w s e. (AsContractError e) => PaymentPubKeyHash -> (TokenName, Integer) -> Contract w s e Recipient
mintContract pk amounts = do
  txOutRef <- getUnspentOutput
  utxos <- utxosAt (pubKeyHashAddress pk Nothing)
  let token = mkRecipient txOutRef amounts
      policy = curVali token
      lookups =
        mintingPolicy policy
          <> Constraints.unspentOutputs utxos

      mintTx =
        mustSpendPubKeyOutput txOutRef
          <> mustMintValue (mkRecipientValue (curSymbol token) token)

  ledgerTx <- submitTxConstraintsWith @Scripts.Any lookups mintTx
  _ <- awaitTxConfirmed (getCardanoTxId ledgerTx)
  return token

sendMintToAll :: RecipientP -> Contract w s Text ()
sendMintToAll RecipientP {_asset = (tn, ammount), _recipients = recips} = do
  ownPK <- Contract.ownFirstPaymentPubKeyHash
  cur <- mintContract ownPK (tn, Haskell.toInteger (Haskell.length recips) * ammount)
  let cs = curSymbol cur
      v = Value.singleton cs tn ammount
  forM_ recips $ \w ->
    case (w /= ownPK) of
      True -> do
        unbalTx <- mkTxConstraints @Void Haskell.mempty (mustPayToPubKey w v)
        adjustTx <- Contract.adjustUnbalancedTx unbalTx
        submitTxConfirmed adjustTx
        logInfo @Haskell.String $ printf "sent tokens to: %s" (Haskell.show w)
      _ -> return ()

mintEndpoints :: Contract () RecipientSchema Text ()
mintEndpoints = do
  awaitPromise mint'
  mintEndpoints
  where
    mint' = endpoint @"mint" $ sendMintToAll

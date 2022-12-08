{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE LambdaCase            #-}

module TokenMint1.Trace where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras
import           Data.Map                   as Map hiding (take)
import           Plutus.Trace.Emulator      as Emulator
import           Ledger.Value               as Value
import           Control.Applicative        as A
import           Data.Default
import           Data.Void
import           Ledger
import qualified Plutus.Contract.Trace      as Trace
import           Wallet.Emulator.Wallet
import           Plutus.Contract            as Contract
import           Wallet.Emulator.Stream
import           Data.Text                  (Text(..), pack)
import           Ledger.Ada
import           Text.Printf
import           TokenMint1.SendToScriptMint

traceIO :: IO ()
traceIO = runEmulatorTraceIO' def conf sendToMintTrace


conf :: EmulatorConfig
conf = def {_initialChainState = Left $ Map.fromList intialDistro}
    where
        intialDistro :: [(Wallet,Value)]
        intialDistro = (\(x,xs) -> (x,lovelaceValueOf xs)) <$> A.liftA2 (,) wallets walletBal
        wallets :: [Wallet]
        wallets = take 4 $ knownWallets
        walletBal :: [Integer]
        walletBal = [100_000_000]


sendToMintTrace :: EmulatorTrace ()
sendToMintTrace = do
    let
        giveParams = MintingParams "SAND" 1000
        w1 = knownWallet 1
        w2 = knownWallet 2
        w3 = knownWallet 3
    h1 <- activateContractWallet w1 endpoints
    -- _ <- activateContractWallet w1 $ void $ give @() @EmptySchema giveParams
    {- Tip: [Contracts without endpoints]
    useful to remember is you don't plan to use endpoints, at least for me anyway ;-)
    -}
    callEndpoint @"give" h1 giveParams
    _ <- Emulator.waitNSlots 3
    Extras.logInfo @String $ printf "called 'give' with params: %s" (show giveParams)
    h2 <- activateContractWallet w2 endpoints
    h3 <- activateContractWallet w3 endpoints
    callEndpoint @"grab" h2 ()
    _ <- Emulator.waitNSlots 3
    callEndpoint @"grab" h3 ()
    _ <- Emulator.waitNSlots 3
    callEndpoint @"grab" h1 ()
    _ <- Emulator.waitNSlots 3
    -- callEndpoint @"grab" h1 ()
    -- _ <- Emulator.waitNSlots 3
    return ()
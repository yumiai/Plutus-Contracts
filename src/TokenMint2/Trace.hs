{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module TokenMint2.Trace where

import           Control.Applicative    as A
import           Data.Default
import           Data.Map               as Map hiding (take)
import           Ledger.Ada
import           Ledger.Value           as Value
import           Plutus.Trace.Emulator  as Emulator
import           TokenMint2.Mint
import           Wallet.Emulator.Wallet

traceIO :: IO ()
traceIO = runEmulatorTraceIO' def conf sendToMintTrace


conf :: EmulatorConfig
conf = def {_initialChainState = Left $ Map.fromList intialDistro}
    where
        intialDistro :: [(Wallet,Value)]
        intialDistro = (\(x,xs) -> (x,lovelaceValueOf xs)) <$> A.liftA2 (,) wallets walletBal
        wallets :: [Wallet]
        wallets = take 5 $ knownWallets
        walletBal :: [Integer]
        walletBal = [1000_000_000]

sendToMintTrace :: EmulatorTrace ()
sendToMintTrace = do
    let
        mintParams = RecipientP ("SAND",200) walletPubkeys
        walletPubkeys = [mockWalletPaymentPubKeyHash wlts | wlts <- (Prelude.drop 1 $ take 5 knownWallets) ]
        w1 = knownWallet 1
    h1 <- activateContractWallet w1 mintEndpoints
    callEndpoint @"mint" h1 mintParams
    _  <- Emulator.waitNSlots 10
    return ()

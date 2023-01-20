{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DataKinds         #-}

module ContractErrors where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import Data.Void                  (Void)
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

contract1 :: Contract () Empty Text ()
contract1 = do
    err <- Contract.throwError "this is an error"
    Contract.logError $ unpack err
    
contractTrace1 :: EmulatorTrace ()
contractTrace1 = do
    _ <- activateContractWallet (knownWallet 1) contract1
    return ()

runTrace1 :: IO ()
runTrace1 = runEmulatorTraceIO contractTrace1

contract2 :: Contract () Empty Text ()
contract2 = Contract.handleError (Contract.logError . unpack) contract1

contractTrace2 :: EmulatorTrace ()
contractTrace2 = do
    let
        w1 = knownWallet 1
    _ <- activateContractWallet w1 contract2
    return ()

runTrace2 :: IO ()
runTrace2 = runEmulatorTraceIO contractTrace2
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
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module StateMachines.TrafficLight where

import PlutusTx
import Data.Aeson                               as A
import Data.Monoid (Last(..))
import Ledger                                   as Ledger
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
import qualified Prelude                        as P
import Plutus.V1.Ledger.Scripts                 as V1
import PlutusTx.Builtins                        as Builtins
import Ledger.Ada                               as Ada
import Prelude                                  (Semigroup (..),Monoid(..))
import PlutusTx.Prelude                         hiding (Monoid (..), Semigroup (..))
import Plutus.Contract.StateMachine

-- Currently blocked until I can figure out how to model a process from which to base my model on.
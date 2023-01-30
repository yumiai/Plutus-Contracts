module Main where

import           Options.Applicative
import           PlutusOptions
import           Utils


main :: IO ()
main =  customExecParser p minterInfo >>= minterExec
    where
        p = prefs showHelpOnEmpty

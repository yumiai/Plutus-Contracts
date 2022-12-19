{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module PlutusOptions where

import           Control.Exception   (throwIO)
import           Data.String         (IsString (fromString))
import           Options.Applicative
import           TokenMint2.Mint     (mkRecipient)
import           Utils               (safeTokenNameToHex, safeTxOut,
                                      writeMintingPolicy)

data MParams = MakeScript !ValidatorParams | SerialseAssetName !String  deriving Show

data ValidatorParams = ValidatorParams
      { assetName    :: !String
      , assetAmmount :: !Integer
      , outputRef    :: !String
      } deriving Show


data MinterParser = MinterParser MParams | Version deriving Show

tokenToHex :: Parser MParams
tokenToHex = SerialseAssetName <$> strArgument (help "serialise asset name to hexadecial values.")

createScript :: Parser MParams
createScript = MakeScript <$> validatorParams

version :: Parser MinterParser
version = flag' Version $ long "version" <> short 'v' <> help "print current version out to console"

scriptCommands :: Parser MinterParser
scriptCommands = MinterParser
        <$> hsubparser (command "script" (info createScript(progDesc "Create a minting script with following arguments"))
                    <> command "serialise" (info tokenToHex (progDesc "serialise token name to a hexadeciaml value"))
                       )

validatorParams :: Parser ValidatorParams
validatorParams = ValidatorParams
        <$> strOption
            (long "name" <>
             metavar "<TARGET>" <>
             help "target name for your token")
        <*> option auto
            (long "ammount" <>
             short 'a'    <>
             metavar "<INTEGER>" <>
             help "Ammount you want minted" )
        <*> strOption
            (long "oref" <>
             short 'o' <>
             metavar "<TARGET>" <>
             help "An unspent output from your wallet containing the TxId and TxIdx seperated by a #")

input :: Parser MinterParser
input = scriptCommands <|> version

minterExec :: MinterParser -> IO ()
minterExec (MinterParser (SerialseAssetName s)) =
        case safeTokenNameToHex (fromString s) of
            Just s -> putStrLn s
            _      -> throwIO $ userError "invalid string"
minterExec (MinterParser (MakeScript ValidatorParams {
        assetName=an
       ,assetAmmount=n
       ,outputRef=rf})) = do
            let
                astname = fromString an
                mkOutRef x = case safeTxOut x of
                    Left err -> throwIO $ userError err
                    Right tx -> return tx
            oref <- mkOutRef rf
            mnt  <- writeMintingPolicy $ mkRecipient oref (astname,n)
            case mnt of
                Left err -> throwIO $ userError (show err)
                Right () -> return ()
minterExec Version = putStrLn "TokenMint2 Script CLI - v0.1.0"

minterInfo :: ParserInfo MinterParser
minterInfo = info (helper <*> input) info'
    where
        info' = briefDesc <>
                header "TokenMint2 CLI" <>
                progDesc "An interface to make token minting scripts" <>
                footer "Made with tlc by parduseidolon"

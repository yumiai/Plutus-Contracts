{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Utils where


import           Cardano.Api                as API
import           Cardano.Api.Shelley        (PlutusScript (..))
import           Codec.Serialise            (serialise)
import           Data.Aeson                 (encode)
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Short      as BS
import           Data.Maybe                 (fromJust)
import           Data.String                (IsString (..))
import           Ledger
import           Plutus.V1.Ledger.Scripts   as V1
import           Plutus.V1.Ledger.Value     as Value
import           PlutusTx                   (Data (..), toData)
import           PlutusTx.Builtins.Internal (BuiltinByteString (..))
import           Text.Printf                (printf)
import           Text.Read                  (readMaybe)
import           TokenMint2.Mint


dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

-- take a tokenname to a hex; hexadecimal bystrings must be used as values for minting fields
safeTokenNameToHex :: TokenName -> Maybe String
safeTokenNameToHex t = B8.unpack . serialiseToRawBytesHex <$> toAsset t
    where
        toAsset :: TokenName -> Maybe AssetName
        toAsset = deserialiseFromRawBytes AsAssetName . (\(BuiltinByteString s) -> s) . unTokenName

writeMintingPolicy :: Recipient -> IO (Either (FileError ()) ())
writeMintingPolicy rcp = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) "tokenMint2.policy" Nothing serialised
    where
        script = getMintingPolicy . policy $ rcp
        serialised = PlutusScriptSerialised . BS.toShort . LBS.toStrict . serialise $ script


safeTxOut :: String -> Either String TxOutRef
safeTxOut output = case checkTx $ txSpan output of
        Right tx@(txid,_) -> Right $ mkTxOut txid (fromJust . checkReadInteger $ tx)
        Left err          -> Left err
    where
        txSpan :: String -> (String,String)
        txSpan xs = span (/= '#') xs

        checkTx :: (String,String) ->  Either String (String,String)
        checkTx ts@(x,xs)
            | not (null x) && not (null xs) = return ts
            | null x && not (null xs) = Left $ printf "You must have a valid TxId: %s" (show ts)
            | not (null x) && null xs = Left $ printf "Invalid TxIndex: %s" (show ts)
            | otherwise = Left $ printf "Invalid TxOutRef: %s" (show ts)

        checkReadInteger :: (String,String) -> Maybe Integer
        checkReadInteger x = case checkTx x of
            Right (_,_:i) -> readMaybe i
            _             -> Nothing

        mkTxOut :: String -> Integer -> TxOutRef
        mkTxOut id = TxOutRef (fromString id)

writeRedeemer :: IO ()
writeRedeemer = LBS.writeFile "unit.json" . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . toData $ ()

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Utils where


import Cardano.Api as API
import Cardano.Api.Shelley (PlutusScript(..))
import Ledger
import PlutusTx
import Plutus.V1.Ledger.Scripts       as V1
import Plutus.V1.Ledger.Value         as Value
import PlutusTx.Builtins.Internal  (BuiltinByteString (..))
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.ByteString.Short as BS
import Codec.Serialise (serialise)
import TokenMint2.Mint

safeTokenNameToHex :: TokenName -> Maybe String
safeTokenNameToHex t = B8.unpack . serialiseToRawBytesHex <$> toAsset t
    where
        toAsset :: TokenName -> Maybe AssetName 
        toAsset = deserialiseFromRawBytes AsAssetName . (\(BuiltinByteString s) -> s) . unTokenName


writeValidator :: MintingPolicy -> IO (Either (FileError ()) ())
writeValidator = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) "tokenMint2.policy" Nothing . PlutusScriptSerialised . BS.toShort . LBS.toStrict . serialise . getMintingPolicy

writeValidator' :: MintingPolicy -> IO (Either (FileError ()) ())
writeValidator' mp = do
    let
        script = getMintingPolicy mp
        serialised = PlutusScriptSerialised . BS.toShort . LBS.toStrict . serialise $ script
    writeFileTextEnvelope @(PlutusScript PlutusScriptV1) "tokenMint2.policy" Nothing serialised
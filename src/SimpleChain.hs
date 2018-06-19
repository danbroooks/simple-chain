module SimpleChain
  ( Blockchain
  , Block
  , printChain
  , createChain
  , writeChain
  ) where

import           Crypto.Hash
import           Data.Aeson
import           Data.ByteArray (ByteArrayAccess(..))
import qualified Data.ByteArray as BA
import           Data.ByteArray.Encoding (Base(Base16))
import qualified Data.ByteArray.Encoding as BA
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe (fromMaybe)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Time.Clock.POSIX (getPOSIXTime)

class ByteStringRepr a where
  reprBS :: a -> BS.ByteString

data Timestamp = Timestamp
  { unTimestamp :: Int
  }

makeTimestamp :: IO Timestamp
makeTimestamp = Timestamp . round <$> getPOSIXTime

instance ByteStringRepr Timestamp where
  reprBS = BS.pack . show . unTimestamp

data Block a
  = Genesis Timestamp
  | Block Timestamp (BlockData a)

instance ToJSON a => ByteStringRepr (Block a) where
  reprBS block =
    case block of
      Genesis ts -> reprBS ts
      Block ts bd -> reprBS ts <> reprBS bd

data BlockData a = BlockData
  { blockValue :: a
  , blockPrevHash :: BS.ByteString
  }

blockValueJSON :: ToJSON a => BlockData a -> BS.ByteString
blockValueJSON = BSL.toStrict . encode . blockValue

instance ToJSON a => ByteStringRepr (BlockData a) where
  reprBS bd = blockValueJSON bd <> blockPrevHash bd

calculateHash :: ToJSON a => Block a -> BS.ByteString
calculateHash = BA.convertToBase Base16 . hashWith SHA256 . reprBS

data Blockchain a = Block a :/ [Block a]

createChain :: IO (Blockchain a)
createChain = do
  timestamp <- makeTimestamp
  pure $ (Genesis timestamp) :/ []

latestBlock :: Blockchain a -> Block a
latestBlock (blk :/ _) = blk

writeChain :: ToJSON a => a -> Blockchain a -> IO (Blockchain a)
writeChain newValue (head :/ tail) = do
  timestamp <- makeTimestamp
  let block = Block timestamp $ BlockData newValue (calculateHash head)
   in pure $ block :/ (head : tail)

printChain :: ToJSON a => Blockchain a -> IO ()
printChain (head :/ tail) = mapM_ printBlock (head : tail)

printBlock :: ToJSON a => Block a -> IO ()
printBlock block =
  case block of
    Genesis timestamp -> do
      putStrLn $ "* " <> hash <> " [genesis]"
      putStrLn ""
    Block timestamp blockdata -> do
      putStrLn $ "* " <> hash <> " [previous: " <> (displayHash . blockPrevHash $ blockdata) <> "]"
      putStrLn ""
      putStrLn "Data:"
      putByteStringLn (blockValueJSON $ blockdata)
      putStrLn ""
  where
    hash = displayHash . calculateHash $ block
    displayHash = take 8 . BS.unpack

putByteStringLn :: BS.ByteString -> IO ()
putByteStringLn = putStrLn . BS.unpack

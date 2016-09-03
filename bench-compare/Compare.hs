module Main (main) where

import Control.Monad (forM, liftM)
import Data.Csv (FromRecord, HasHeader (HasHeader), (.!), decode, parseRecord)
import System.Directory (listDirectory)
import System.FilePath ((</>), dropExtensions)
import qualified Data.ByteString.Lazy as B
import qualified Data.Vector as V


main :: IO ()
main = do
  files <- listDirectory "data"
  records <- forM files readData
  print records
  return ()

readData :: FilePath -> IO (String, V.Vector (String, Double))
readData file = fmap (dropExtensions file, ) records where
  records = liftM decodeRecords $ B.readFile ("data" </> file)

decodeRecords :: B.ByteString -> V.Vector (String, Double)
decodeRecords = either error (V.map unRecord) . decode HasHeader

newtype DataRecord = Record { unRecord :: (String, Double) }

instance FromRecord DataRecord where
  parseRecord x = fmap Record $ (,) <$> (x .! 0) <*> (x .! 1)

module Main (main) where

import Control.Monad (forM)
import System.Directory (listDirectory)
import System.FilePath ((</>), dropExtensions)
import qualified Data.ByteString.Lazy as B
import qualified Data.Csv as C
import qualified Data.Vector as V


main :: IO ()
main = do
  files <- listDirectory "data"
  records <- forM files readData
  print records
  return ()

readData :: FilePath -> IO (String, V.Vector (String, Double))
readData file = do
  content <- B.readFile ("data" </> file)
  return (dropExtensions file, decodeRecords content)

decodeRecords :: B.ByteString -> V.Vector (String, Double)
decodeRecords = either error (V.map unRecord) . C.decode C.HasHeader

newtype Record = Record { unRecord :: (String, Double) }

instance C.FromRecord Record where
  parseRecord x = fmap Record $ (,) <$> (x C..! 0) <*> (x C..! 1)

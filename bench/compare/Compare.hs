module Main (main) where

import Control.Monad (forM)
import Data.List (sortOn)
import Data.Maybe (maybe)
import System.Directory (listDirectory)
import System.FilePath ((</>), splitExtension)
import Text.PrettyPrint.Boxes (Box, (<>), (//), printBox, text)
import qualified Data.ByteString.Lazy as B
import qualified Data.Csv as C
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V


main :: IO ()
main = do
  let dir = "../data"
  files <- listDirectory dir
  benchmarks <- fmap (sortOn fst) $ forM files (readData dir)
  putStrLn ""
  let suits = map fst benchmarks
  let (cases, measures) = combineRecords $ map snd benchmarks
  let x <|> y = x <> column (replicate (length cases + 2) " | ") <> y
  let benchColumn = "benchmark" // "-----" // column cases
  printBox $ benchColumn <|> foldr1 (<|>) (zipWith timeColumn suits measures)
  putStrLn ""

timeColumn :: String -> [Maybe Double] -> Box
timeColumn suit means = text suit // "-----" // column (map showTime means)

showTime :: Maybe Double -> String
showTime = maybe "" (show . (round :: Double -> Int) . (* 1000000000))

column :: [String] -> Box
column = foldr1 (//) . map text


type Record = (String, Double)

combineRecords :: [V.Vector Record] -> ([String], [[Maybe Double]])
combineRecords records = (names, map means records) where
  means = map snd . M.toAscList . flip M.union namesMap . M.fromList . V.toList . V.map (fmap Just)
  namesMap = M.fromAscList $ zip names (repeat Nothing)
  names = S.toAscList $ S.unions $ map (S.fromList . V.toList . V.map fst) records


readData :: FilePath -> FilePath -> IO (String, V.Vector Record)
readData dir file = do
  content <- B.readFile (dir </> file)
  return (fst $ splitExtension file, decodeRecords content)

decodeRecords :: B.ByteString -> V.Vector Record
decodeRecords = either error (V.map unRecord) . C.decode C.HasHeader

newtype CsvRecord = CsvRecord { unRecord :: Record }

instance C.FromRecord CsvRecord where
  parseRecord x = fmap CsvRecord $ (,) <$> (x C..! 0) <*> (x C..! 1)

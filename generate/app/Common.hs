module Common where

import Data.DoList (DoList, fromList, item, toList)


type Text = String
type Line = DoList Text


pragmaFunc :: Text -> Text -> Text -> Line
pragmaFunc pragma func body = do
    line $ "{-#" +++ pragma +++ func +++ "#-}"
    line $ func +++ body


hotMatching :: Int -> Text
hotMatching n = "(" ++ hotConstr n (("x" ++) . show) ++ ")"

hotConstr :: Int -> (Int -> Text) -> Text
hotConstr n f = "Hot" ++ show n +++ unwords (map f [1 .. n])


line :: Text -> Line
line = item

runLines :: Line -> Text
runLines = unlines . toList

forN :: (Monad m) => Int -> (Int -> m a) -> m a
forN n f = foldr1 (>>) $ map f [1 .. n]

(+++) :: Text -> Text -> Text
x +++ y = x ++ " " ++ y

indent :: Line -> Line
indent = fromList . map ("    " ++) . toList

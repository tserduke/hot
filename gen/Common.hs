module Common where

import Data.DoList (DoList, fromList, toList)


type Text = String
type Lines = DoList Text

type Function = Text -> Text -> Lines


pragmaFunc :: Text -> Function
pragmaFunc pragma func body = do
    ["{-#" +++ pragma +++ func +++ "#-}"]
    [func +++ body]


hotMatching :: Int -> Text
hotMatching n = "(" ++ hotConstr n (("x" ++) . show) ++ ")"

hotConstr :: Int -> (Int -> Text) -> Text
hotConstr n f = "Hot" ++ show n +++ unwords (map f [1 .. n])


indent :: Lines -> Lines
indent = fromList . map ("    " ++) . toList

forN :: (Monad m) => Int -> (Int -> m a) -> m a
forN n f = foldr1 (>>) $ map f [1 .. n]

(+++) :: Text -> Text -> Text
x +++ y = x ++ " " ++ y


runLines :: Lines -> Text
runLines = unlines . toList

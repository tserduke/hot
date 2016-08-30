module Data.Hot.Internal
  ( hotError
  ) where


{-# NOINLINE hotError #-}
hotError :: (Show a) => Int -> String -> a -> e
hotError n func x = error $ concat $ ["Hot", show n, " ", func, " ", show x]

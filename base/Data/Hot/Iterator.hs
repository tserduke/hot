module Data.Hot.Iterator
  ( Iterator (Iter)
  , iterLeft
  , iterRight
  , iterNext
  , iterYield
  ) where


data Iterator a = Iter !a (Iterator a)


{-# INLINABLE iterLeft #-}
iterLeft :: (Foldable t) => t a -> Iterator a
iterLeft = foldl (flip Iter) undefined

{-# INLINABLE iterRight #-}
iterRight :: (Foldable t) => t a -> Iterator a
iterRight = foldr Iter undefined

{-# INLINABLE iterNext #-}
iterNext :: Iterator a -> Iterator a
iterNext (Iter _ ix) = ix

{-# INLINABLE iterYield #-}
iterYield :: Iterator a -> a
iterYield (Iter x _) = x

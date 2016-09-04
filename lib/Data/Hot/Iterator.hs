module Data.Hot.Iterator
  ( Iterator (Iter)
  , inext
  , iyield
  ) where


data Iterator a = Iter a (Iterator a)


inext :: Iterator a -> Iterator a
inext (Iter _ ix) = ix

iyield :: Iterator a -> a
iyield (Iter x _) = x

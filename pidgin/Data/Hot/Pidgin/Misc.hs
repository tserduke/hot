module Data.Hot.Pidgin.Misc where

import Data.Hot.Pidgin.Base


prefix5of10, suffix5of10 :: PHot10 a -> PHot5 a
prefix5of10 (PHot10 x1 x2 x3 x4 x5 _ _ _ _ _) = PHot5 x1 x2 x3 x4 x5

suffix5of10 (PHot10 _ _ _ _ _ x1 x2 x3 x4 x5) = PHot5 x1 x2 x3 x4 x5


merge2and3 :: (Ord a) => PHot2 a -> PHot3 a -> PHot5 a
merge2and3 (PHot2 x1 x2) (PHot3 y1 y2 y3) = if x1 < y1
  then if x2 < y1 --x1
       then PHot5 x1 x2 y1 y2 y3
       else if x2 < y2 --x1 y1
            then PHot5 x1 y1 x2 y2 y3
            else if x2 < y3 --x1 y1 y2
                 then PHot5 x1 y1 y2 x2 y3
                 else PHot5 x1 y1 y2 y3 x2
  else if x1 < y2 --y1
       then if x2 < y2 --y1 x1
            then PHot5 y1 x1 x2 y2 y3
            else if x2 < y3 --y1 x1 y2
                 then PHot5 y1 x1 y2 x2 y3
                 else PHot5 y1 x1 y2 y3 x2
       else if x1 < y3 --y1 y2
            then if x2 < y3 --y1 y2 x1
                 then PHot5 y1 y2 x1 x2 y3
                 else PHot5 y1 y2 x1 y3 x2
            else PHot5 y1 y2 y3 x1 x2

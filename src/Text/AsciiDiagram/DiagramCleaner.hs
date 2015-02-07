{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE CPP #-}
module Text.AsciiDiagram.DiagramCleaner
    ( isShapePossible
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( Applicative, (<*>) )
#endif

import Control.Applicative( (<$>), liftA2 )
import Data.List( tails )
import Text.AsciiDiagram.Geometry
import Linear( V2( V2 )
             , (^-^)
             )

compareDirections :: Applicative f
                  => (Int -> Int -> Bool) -> f Int -> f Int -> f Bool
compareDirections f = liftA2 diffSign
  where
    diffSign 0 0 = True
    diffSign aa bb = f aa bb

checkRoundedCorners :: (Int -> Int -> Bool)
                    -> Segment -> Point -> Point -> Segment
                    -> Bool
checkRoundedCorners f s1 ap1 ap2 s2 = okX && okY
  where
    V2 dirX dirY = ap2 ^-^ ap1
    fromS1 = _segEnd s1 ^-^ ap1
    fromS2 = _segStart s2 ^-^ ap2 

    signDirs = signum <$> V2 dirY dirX

    V2 okX okY = (&&)
       <$> (compareDirections f signDirs (signum <$> fromS1))
       <*> (compareDirections f signDirs (signum <$> fromS2))

checkClosedShape :: Shape -> Bool
checkClosedShape shape = all checkClosed elements where
  elements =
    (++ shapeElements shape) <$> tails (shapeElements shape) 



checkClosed :: [ShapeElement] -> Bool
--   dir              fromS1       fromS1
--  --->             ----->       <----               ^ 2 \--- 3
--   /\         | 1 /---- 0      0 ----/ 1 |          |    \
-- 1/  \2   dir |  /                  /    | dir  dir |    /
--  |  |        |  \                  \    |          | 1 /--- 0
-- 0|  |3       v 2 \---- 3      3 ----\ 2 v
--                   ----->       <----
--                    fromS2      fromS2
-- 
--   OK                OK         BAD                    BAD
checkClosed
      ( ShapeSegment s1
      : ShapeAnchor ap1 AnchorFirstDiag   -- '/'
      : ShapeAnchor ap2 AnchorSecondDiag  -- '\'
      : ShapeSegment s2
      : _) = checkRoundedCorners (==) s1 ap1 ap2 s2

--   dir              fromS1       fromS1
--  --->             ----->       <----               ^ 1 \--- 0
--   /\         | 2 /---- 3      3 ----/ 2 |          |    \
-- 2/  \1   dir |  /                  /    | dir  dir |    /
--  |  |        |  \                  \    |          | 2 /--- 3
-- 3|  |0       v 1 \---- 0      0 ----\ 1 v
--                   ----->       <----
--                    fromS2      fromS2
-- 
--   OK                OK         BAD                    BAD
checkClosed
      ( ShapeSegment s1
      : ShapeAnchor ap1 AnchorSecondDiag  -- '\'
      : ShapeAnchor ap2 AnchorFirstDiag   -- '/'
      : ShapeSegment s2
      : _) = checkRoundedCorners (/=) s1 ap1 ap2 s2

checkClosed
      ( ShapeAnchor _ AnchorFirstDiag
      : ShapeAnchor _ AnchorSecondDiag
      : ShapeAnchor _ AnchorFirstDiag
      : ShapeAnchor _ AnchorSecondDiag
      : _) = False

checkClosed
      ( ShapeAnchor _ AnchorSecondDiag
      : ShapeAnchor _ AnchorFirstDiag
      : ShapeAnchor _ AnchorSecondDiag
      : ShapeAnchor _ AnchorFirstDiag
      : _) = False

checkClosed _ = True

isBullet :: ShapeElement -> Bool
isBullet (ShapeAnchor _ AnchorBullet) = True
isBullet _ = False

checkOpened :: [ShapeElement] -> Bool
checkOpened
      [ ShapeAnchor _ AnchorFirstDiag
      , ShapeAnchor _ AnchorSecondDiag] = False
checkOpened [ ShapeAnchor _ AnchorSecondDiag
            , ShapeAnchor _ AnchorFirstDiag] = False
checkOpened (all isBullet -> True) = False
checkOpened _ = True

checkOpenedShape :: Shape -> Bool
checkOpenedShape = checkOpened . shapeElements

isShapePossible :: Shape -> Bool
isShapePossible shape
    | shapeIsClosed shape = checkClosedShape shape
    | otherwise = checkOpenedShape shape


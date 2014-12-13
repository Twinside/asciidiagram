module Text.AsciiDiagram.Deduplicator( deduplicate ) where

import Control.Applicative
import Data.Function( on )
import Data.List( partition
                , inits
                , tails
                , minimumBy
                )
import qualified Data.Set as S
import Text.AsciiDiagram.Geometry
import Linear( V2( V2 ), (^-^) )

isNearBy :: Point -> Point -> Bool
isNearBy p1 p2 = check $ abs <$> p1 ^-^ p2
  where check (V2 a b) = a + b <= 1

rotateLeft :: [a] -> [a]
rotateLeft [] = []
rotateLeft (x:xs) = xs ++ [x]

allRotations :: [a] -> [[a]]
allRotations l =
    init . zipWith (++) (tails l) $ inits l

signedAreaOfPoints :: [Point] -> Int
signedAreaOfPoints lst =
  sum [x1 * y2 - x2 * y1
            | (V2 x1 y1, V2 x2 y2) <- zip lst $ rotateLeft lst]

pointsOfShape :: Shape -> [Point]
pointsOfShape = go Nothing . shapeElements where
  go _ [] = []
  go _ (ShapeAnchor p _:rest) = p : go (Just p) rest
  go _ (ShapeSegment seg:rest)
    | start == _segEnd seg = start : go (Just start) rest
      where start = _segStart seg
  go (Just prev) (ShapeSegment seg:rest)
    | prev `isNearBy` start = start : end : go (Just end) rest
    | otherwise = end : start : go (Just start) rest
      where start = _segStart seg
            end = _segEnd seg
  go Nothing (ShapeSegment seg:rest@(nextShape:_)) =
    case nextShape of
      ShapeAnchor p  _ | p `isNearBy` start ->
          end : start : go (Just start) rest
      ShapeAnchor p _ -> p : go (Just p) rest
      ShapeSegment nextSeg ->
          matchSegmentBounds start end nextSeg rest
      where start = _segStart seg
            end = _segEnd seg
  go Nothing [ShapeSegment _] =
      error "Ill-condisioned shape"

  matchSegmentBounds start end nextSeg after
    | start `isNearBy` _segStart nextSeg ||
      start `isNearBy` _segEnd nextSeg =
          end : start : go (Just start) after
    | otherwise = start : end : go (Just end) after
  
makeShapeClockwise :: Shape -> Shape
makeShapeClockwise shape
    | signedArea < 0 = Shape (reverse $ shapeElements shape) True
    | otherwise = shape
  where
    signedArea = signedAreaOfPoints $ pointsOfShape shape

firstPointOfShape :: [ShapeElement] -> Point
firstPointOfShape lst = case lst of
  [] -> V2 (-10) (-10)
  ShapeAnchor p _ : _ -> p
  ShapeSegment seg : _ -> _segStart seg

alignOnLowerFirstPoint :: Shape -> Shape
alignOnLowerFirstPoint shape = Shape newElems $ shapeIsClosed shape
  where
    newElems = minimumBy (compare `on` firstPointOfShape)
             . allRotations $ shapeElements shape

normalizeClosedShapes :: [Shape] -> [Shape]
normalizeClosedShapes =
    fmap $ alignOnLowerFirstPoint . makeShapeClockwise

deduplicate :: [Shape] -> S.Set Shape
deduplicate lst = S.fromList $ open ++ normalizeClosedShapes closed
  where
    (closed, open) = partition shapeIsClosed lst


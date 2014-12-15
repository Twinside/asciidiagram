module Text.AsciiDiagram.Deduplicator( normalizeClosedShape
                                     , removeLargeCycle 
                                     ) where

import Control.Applicative
import Data.Foldable( Foldable )
import qualified Data.Foldable as F
import Data.Function( on )
import Data.List( inits
                , tails
                , minimumBy
                , sort
                )
import Data.Monoid( mempty )
import qualified Data.Set as S
import Linear( V2( V2 ), (^-^) )

import Text.AsciiDiagram.Geometry

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

alignOnLowerFirstPoint :: Shape -> Shape
alignOnLowerFirstPoint shape = Shape newElems $ shapeIsClosed shape
  where
    newElems = minimumBy (compare `on` firstPointOfShape)
             . allRotations $ shapeElements shape

normalizeClosedShape :: Shape -> Shape
normalizeClosedShape =
    alignOnLowerFirstPoint . makeShapeClockwise

removeLargeCycle :: Foldable t => t Shape -> S.Set Shape
removeLargeCycle =
    snd . removeCycleWithAllUsedElements . sortShapesByArea
  where
    removeCycleWithAllUsedElements =
      F.foldl' go (mempty, mempty)

    sortShapesByArea = 
      fmap snd . sort . fmap addArea . F.toList

    addArea shape =
      (abs . signedAreaOfPoints $ pointsOfShape shape, shape)

    isAllUsed allElems =
      all (`S.member` allElems) . shapeElements

    enrichWith set =
      F.foldl' (flip S.insert) set . shapeElements

    go (seen, out) shape
      | isAllUsed seen shape = (seen, out)
      | otherwise = (seen `enrichWith` shape, S.insert shape out)


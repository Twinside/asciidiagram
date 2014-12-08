module Text.AsciiDiagram.Reconstructor  where

import Control.Applicative( (<$>) )
import Text.AsciiDiagram.Geometry
import Data.Foldable as F
import Linear( V2( .. ), (^+^), (^-^) )
import qualified Data.Set as S
import qualified Data.Map as M

data ShapeElement
    = ShapeAnchor Point Anchor
    | ShapeSegment Segment
    deriving (Eq, Ord, Show)

data Direction
  = LeftToRight
  | RightToLeft
  | TopToBottom
  | BottomToTop
  | NoDirection
  deriving (Eq, Show)

data Shape = Shape
    { shapeElements :: [ShapeElement]
    , shapeIsClosed :: Bool
    }
    deriving (Eq, Show)

prepareLocationMap :: M.Map Point Anchor -> S.Set Segment
                   -> M.Map Point ShapeElement
prepareLocationMap anchors segments =
    F.foldl' addSegment mapWithAnchors segments
  where
    mapWithAnchors = M.mapWithKey ShapeAnchor anchors
    addSegment acc seg = 
        M.insert (_segEnd seg) element $ M.insert (_segStart seg) element acc
      where
        element = ShapeSegment seg

directionOfVector :: Vector -> Direction
directionOfVector (V2 0 n)
  | n > 0 = TopToBottom
  | otherwise = BottomToTop
directionOfVector (V2 n 0)
  | n > 0 = LeftToRight
  | otherwise = RightToLeft
directionOfVector _ = NoDirection

segmentDirection :: Segment -> Vector
segmentDirection seg = 
    signum $ _segStart seg ^-^ _segEnd seg

vectorsForAnchor :: Anchor -> Direction -> [Vector]
vectorsForAnchor a dir = case (a, dir) of
  (AnchorMulti, _) -> [up, right, down, left]
  (AnchorSecondDiag, _) ->
      negate <$> vectorsForAnchor AnchorFirstDiag dir

  (AnchorFirstDiag, LeftToRight) -> [up]
  (AnchorFirstDiag, TopToBottom) -> [left]
  (AnchorFirstDiag, RightToLeft) -> [down]
  (AnchorFirstDiag, BottomToTop) -> [right]
  (AnchorFirstDiag, NoDirection) -> []

  where
    left = V2 (-1) 0
    up = V2 0 (-1)
    right = V2 1 0
    down = V2 0 1

nextPointAfterSegment :: Point -> Segment -> [Point]
nextPointAfterSegment p seg =
    [nextPoint | delta <- [dir, negate <$> dir]
               , let nextPoint = p ^+^ delta
               , nextPoint /= p]
  where
    dir = segmentDirection seg

nextPointAfterAnchor :: Point -> Point -> Anchor -> [Point]
nextPointAfterAnchor pointSource anchorPosition anchor =
    [nextPoint | delta <- deltas
               , let nextPoint = anchorPosition ^+^ delta
               , nextPoint /= previousPoint]
  where
    directionVector = signum $ anchorPosition ^-^ pointSource
    previousPoint = anchorPosition ^-^ directionVector
    direction = directionOfVector directionVector
    deltas = vectorsForAnchor anchor direction

isDirectionIndependant :: ShapeElement -> Bool
isDirectionIndependant (ShapeSegment _) = True
isDirectionIndependant (ShapeAnchor _ anchor) =
  case anchor of
    AnchorMulti -> True
    AnchorFirstDiag -> False
    AnchorSecondDiag -> False

-- | The coordinates on the grid are sure to be in the
-- range ([0..width], [0..height]), so taking negative
-- values ensure that we are in the grid.
unknownStartPoint :: Point
unknownStartPoint = V2 (-10) (-10)

reconstruct :: M.Map Point Anchor -> S.Set Segment -> [Shape]
reconstruct anchors segments = []
  where
    locations = prepareLocationMap anchors segments
    starters =
        S.fromList . filter isDirectionIndependant $ M.elems locations


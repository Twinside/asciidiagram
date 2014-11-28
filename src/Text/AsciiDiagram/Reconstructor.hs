module Text.AsciiDiagram.Reconstructor  where

import Text.AsciiDiagram.Geometry
import Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Map as M

data ShapeElement
    = ShapeAnchor Point Anchor
    | ShapeSegment Segment
    deriving (Eq, Ord, Show)

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

reconstruct :: M.Map Point Anchor -> S.Set Segment -> [Shape]
reconstruct _anchors _segments = []


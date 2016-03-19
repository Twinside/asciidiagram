module Text.AsciiDiagram.BoundingBoxEstimation where

import Data.Monoid( (<>) )
import Linear( V2( .. )
             , (^+^)
             , (^-^)
             , (^*)
             , perp
             , normalize
             )

import Graphics.Svg.Types
import Graphics.Svg.CssTypes

data BoundingBox = BoundingBox
  { _boundingLow    :: !RPoint
  , _boundingHight  :: !RPoint
  }
  deriving (Eq, Show)

instance Monoid BoundingBox where
  mempty = BoundingBox (V2 0 0) (V2 0 0)
  mappend (BoundingBox min1 max1) (BoundingBox min2 max2) =
    BoundingBox (min <$> min1 <*> min2) (max <$> max1 <*> max2)

toEstimatedLength :: Number -> Coord
toEstimatedLength n = case toUserUnit 96 n of
  Num p -> p
  _ -> 0

toEstimatedPoint :: (Number, Number) -> RPoint
toEstimatedPoint (a, b) = V2 a' b' where
  a' = toEstimatedLength a
  b' = toEstimatedLength b

toB :: RPoint -> BoundingBox
toB p = BoundingBox p p

class WithBoundingBox a where
  boundingBoxOf :: a -> BoundingBox

instance WithBoundingBox PolyLine where
  boundingBoxOf = foldMap toB . _polyLinePoints

instance WithBoundingBox Polygon where
  boundingBoxOf = foldMap toB . _polygonPoints

instance WithBoundingBox Line where
  boundingBoxOf (Line _ p1 p2) =
      toB (toEstimatedPoint p1) <> toB (toEstimatedPoint p2)

instance WithBoundingBox Rectangle where
  boundingBoxOf (Rectangle _ base w h _) =
      toB base' <> toB (base' ^+^ toEstimatedPoint (w, h))
     where base' = toEstimatedPoint base

pointOfPath :: PathCommand -> [RPoint]
pointOfPath c = case c of
 MoveTo _ l -> l
 LineTo _ l -> l
 HorizontalTo _ _ -> mempty
 VerticalTo   _ _ -> mempty
 CurveTo _ l -> mconcat [[a, b, c] | (a, b, c) <- l]
 SmoothCurveTo _ l -> mconcat [[a, b] | (a, b) <- l]
 QuadraticBezier _ l -> mconcat [[a, b] | (a, b) <- l]
 SmoothQuadraticBezierCurveTo _ l -> l
 EllipticalArc _ _ -> mempty
 EndPath -> mempty

instance WithBoundingBox Path where
  boundingBoxOf (Path _ p) = foldMap (foldMap toB . pointOfPath) p

instance WithBoundingBox a => WithBoundingBox (Group a) where
  boundingBoxOf (Group _ child _) = foldMap boundingBoxOf child

instance WithBoundingBox a => WithBoundingBox (Symbol a) where
  boundingBoxOf (Symbol g) = boundingBoxOf g

instance WithBoundingBox Tree where
  boundingBoxOf t = case t of
    None            -> mempty
    UseTree _ _     -> mempty
    PathTree p      -> boundingBoxOf p
    CircleTree c    -> boundingBoxOf c
    PolyLineTree pl -> boundingBoxOf pl
    PolygonTree po  -> boundingBoxOf po
    EllipseTree e   -> boundingBoxOf e
    LineTree l      -> boundingBoxOf l
    RectangleTree r -> boundingBoxOf r
    TextTree    _ _ -> mempty
    ImageTree _     -> mempty
    GroupTree g     -> boundingBoxOf g
    SymbolTree s    -> boundingBoxOf s

instance WithBoundingBox Circle where
  boundingBoxOf (Circle _ center rad) = BoundingBox (c ^-^ r) (c ^+^ r)
    where
     c = toEstimatedPoint center
     r = pure $ toEstimatedLength rad

instance WithBoundingBox Ellipse where
  boundingBoxOf (Ellipse _ center xrad yrad) = BoundingBox (c ^-^ r) (c ^+^ r)
    where
     c = toEstimatedPoint center
     r = V2 (toEstimatedLength xrad) (toEstimatedLength yrad)


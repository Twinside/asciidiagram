module Text.AsciiDiagram.SvgRender( shapesToSvgDocument ) where

import Control.Applicative( (<$>) )
import Data.Monoid( Last( .. )
                  , mempty )
import Control.Monad.State.Strict( execState )
import Graphics.Svg.Types( HasDrawAttributes( .. )
                         , Texture( ColorRef )
                         , Document( .. )
                         , drawAttr )
import Codec.Picture( PixelRGBA8( PixelRGBA8 ) )
import qualified Graphics.Svg.Types as Svg
import qualified Data.Set as S
import Linear( V2( .. )
             , (^+^)
             , (^-^)
             , (^*)
             , perp
             , normalize
             )
import Control.Lens( zoom, (.=) )

import Text.AsciiDiagram.Geometry

data GridSize = GridSize
  { _gridCellWidth        :: !Float
  , _gridCellHeight       :: !Float
  , _gridShapeContraction :: !Float
  }
  deriving (Eq, Show)

toSvg :: GridSize -> Point -> Svg.RPoint
toSvg s (V2 x y) =
  V2 (_gridCellWidth s * fromIntegral x)
     (_gridCellHeight s * fromIntegral y)

applyDefaultShapeDrawAttr :: (Svg.WithDrawAttributes a) => a -> a
applyDefaultShapeDrawAttr = execState . zoom drawAttr $ do
    fillColor .= toLC 200 200 200 255
    strokeColor .= toLC 0 0 0 255
    strokeWidth .= toL (Svg.Num 1)
  where
    toL = Last . Just
    toLC r g b a =
        toL . ColorRef $ PixelRGBA8 r g b a

applyDefaultLineDrawAttr :: (Svg.WithDrawAttributes a) => a -> a
applyDefaultLineDrawAttr = execState . zoom drawAttr $ do
    fillColor .= toL Svg.FillNone
    strokeColor .= toLC 0 0 0 255
    strokeWidth .= toL (Svg.Num 1)
  where
    toL = Last . Just
    toLC r g b a =
        toL . ColorRef $ PixelRGBA8 r g b a

startPointOf :: ShapeElement -> Point
startPointOf (ShapeAnchor p _) = p
startPointOf (ShapeSegment seg) = _segStart seg

manathanDistance :: Point -> Point -> Int
manathanDistance a b = x + y where
  V2 x y = abs <$> a ^-^ b

isNearBy :: Point -> Point -> Bool
isNearBy a b = manathanDistance a b <= 1

initialPrevious :: Bool -> [ShapeElement] -> Maybe Point
initialPrevious False _ = Nothing
initialPrevious True [] = Nothing
initialPrevious True lst@(x:_) = Just point
  where
    sp = startPointOf x

    point = case last lst of
      ShapeAnchor pp _ -> pp
      ShapeSegment seg
        | manathanDistance sp (_segStart seg) <
          manathanDistance sp (_segEnd seg) -> _segStart seg
      ShapeSegment seg -> _segEnd seg



swapSegment :: Segment -> Segment
swapSegment seg =
  seg { _segStart = _segEnd seg, _segEnd = _segStart seg }

reorderShapePoints :: Shape -> [(Maybe Point, ShapeElement)]
reorderShapePoints shape = outList where
  outList = go initialPrev elements 
  elements = shapeElements shape
  initialPrev = initialPrevious (shapeIsClosed shape) elements

  go _ [] = []
  go prev (a@(ShapeAnchor p _):rest) =
      (prev, a) : go (Just p) rest
  go prev (s@(ShapeSegment seg):rest)
    | start == _segEnd seg = (prev, s) : go (Just start) rest
      where start = _segStart seg
  go prev@(Just prevPoint) (s@(ShapeSegment seg):rest)
    | prevPoint `isNearBy` start = 
        (prev, s) : go (Just end) rest
    | otherwise = 
        (prev, ShapeSegment $ swapSegment seg) : go (Just start) rest
      where start = _segStart seg
            end = _segEnd seg

  go Nothing (s@(ShapeSegment seg):rest@(nextShape:_)) =
    case nextShape of
      ShapeAnchor p _ | p `isNearBy` start ->
          (Nothing, ShapeSegment $ swapSegment seg) : go (Just start) rest
      ShapeAnchor _ _ ->
          (Nothing, s) : go (Just $ _segEnd seg) rest
      ShapeSegment _ -> (Nothing, s) : go (Just $ _segEnd seg) rest
      where start = _segStart seg
  go Nothing [e@(ShapeSegment _)] = [(Nothing, e)]

associateNextPoint :: Bool -> [(a, ShapeElement)]
                   -> [(a, ShapeElement, Maybe Point)]
associateNextPoint isClosed elements = go elements where
  startingPoint =
    Just . startPointOf . head $ map snd elements

  go [] = []
  go [(p, s)] 
    | isClosed = [(p, s, startingPoint)]
    | otherwise = [(p, s, Nothing)]
  go ((p, s):xs@((_, y):_)) =
    (p, s, Just $ startPointOf y) : go xs

-- >
-- >       ^ perp:(0, -n)
-- >       |
-- > (x, y)|                  (x + n, y)
-- >       +-------------------+ b
-- >      a|
-- >       v correction
--
correctionVectorOf :: Integral a => GridSize -> V2 a -> V2 a -> V2 Float
correctionVectorOf size a b = normalize dir ^* _gridShapeContraction size
  where
    dir = fromIntegral . negate <$> perp (b ^-^ a)

startPoint :: GridSize -> [(Maybe Point, ShapeElement, Maybe Point)]
           -> Svg.RPoint
startPoint gscale shapeElems = case shapeElems of
    [] -> V2 0 0
    (before, ShapeSegment seg, _):_ -> pp ^+^ vc where
       vc = segmentCorrectionVector gscale before seg
       pp = toS $ _segStart seg 
    (Just before, ShapeAnchor p _, Just after):_ -> toS p ^+^ combined
        where v1 = correctionVector before p
              v2 = correctionVector p after
              combined | v1 == v2 = v1
                       | otherwise = v1 ^+^ v2
    (_, ShapeAnchor p _, _):_ -> toS p
  where
    correctionVector = correctionVectorOf gscale
    toS = toSvg gscale

anchorCorrection :: GridSize -> Point -> Point -> Point
                 -> Svg.RPoint
anchorCorrection scale before p after
  | v1 == v2 = v1
  | otherwise = v1 ^+^ v2
  where v1 = correctionVectorOf scale before p
        v2 = correctionVectorOf scale p after

moveTo, lineTo :: Svg.RPoint -> Svg.Path
moveTo p = Svg.MoveTo Svg.OriginAbsolute [p]
lineTo p = Svg.LineTo Svg.OriginAbsolute [p]

smoothCurveTo :: Svg.RPoint -> Svg.RPoint -> Svg.Path
smoothCurveTo p1 p =
  Svg.SmoothCurveTo Svg.OriginAbsolute [(p1, p)]

shapeClosing :: Shape -> [Svg.Path]
shapeClosing Shape { shapeIsClosed = True } = [Svg.EndPath]
shapeClosing _ = []

segmentCorrectionVector :: GridSize -> Maybe Point -> Segment -> Svg.RPoint
segmentCorrectionVector gscale before seg | _segStart seg == _segEnd seg =
  case (before, _segKind seg) of
    (Just v1, _) -> correctionVectorOf gscale v1 (_segEnd seg)
    (Nothing, SegmentHorizontal) -> V2 0 $ _gridShapeContraction gscale
    (Nothing, SegmentVertical) -> V2 (negate $ _gridShapeContraction gscale) 0
segmentCorrectionVector gscale _ seg =
    correctionVectorOf gscale (_segStart seg) (_segEnd seg)

shapeToTree :: GridSize -> Shape -> Svg.Tree
shapeToTree gscale shape =
    Svg.Path $ Svg.PathPrim mempty pathCommands where
  toS = toSvg gscale
  correctionVector = correctionVectorOf gscale

  shapeElems = associateNextPoint (shapeIsClosed shape)
             $ reorderShapePoints shape

  pathCommands =
    moveTo (startPoint gscale shapeElems)
        : fmap toPath shapeElems ++ shapeClosing shape

  toPath (before, ShapeSegment seg, _) = lineTo $ vc ^+^ toS (_segEnd seg)
    where vc = segmentCorrectionVector gscale before seg
  toPath (before, ShapeAnchor p a, after) = case a of
      AnchorPoint -> straightCorner before p after
      AnchorMulti -> straightCorner before p after
      AnchorFirstDiag -> curveCorner before p after
      AnchorSecondDiag -> curveCorner before p after

  straightCorner (Just before) p (Just after) | shapeIsClosed shape =
      lineTo $ anchorCorrection gscale before p after ^+^ toS p
  straightCorner (Just before) p _ | shapeIsClosed shape =
      lineTo $ correctionVector before p ^+^ toS p
  straightCorner _ p _ = lineTo $ toS p

  curveCorner _ p (Just after) =
      smoothCurveTo (toS p) $ toS after ^+^ correction
    where correction = correctionVectorOf gscale p after
  curveCorner (Just before) p Nothing = smoothCurveTo (toS p) $ toS p ^+^ vec
    where vec = correctionVector before p
  curveCorner _ p _ = lineTo $ toS p

shapesToSvgDocument :: (Int, Int) -> S.Set Shape -> Svg.Document
shapesToSvgDocument (w, h) shapes = Document	 
  { _viewBox = Nothing
  , _width = toSvgSize _gridCellWidth w
  , _height = toSvgSize _gridCellHeight h
  , _elements = closedSvg ++ lineSvg
  , _definitions = mempty
  , _description = ""
  , _styleText  = ""
  , _styleRules = []
  }
  where
    (closed, opened) = S.partition shapeIsClosed shapes
    closedSvg =
        applyDefaultShapeDrawAttr . shapeToTree scale <$> S.toList closed
    lineSvg = 
        applyDefaultLineDrawAttr . shapeToTree strokeScale <$> S.toList opened

    toSvgSize accessor var =
        Just . Svg.Num $ fromIntegral var * accessor scale + 5

    strokeScale = scale { _gridShapeContraction = 0 }
    scale = GridSize
      { _gridCellWidth = 11
      , _gridCellHeight = 14
      , _gridShapeContraction = 1.5
      }


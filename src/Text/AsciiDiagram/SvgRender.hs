module Text.AsciiDiagram.SvgRender( svgOfDiagram ) where

import Control.Applicative( (<$>) )
import Control.Monad.State.Strict( execState )
import Data.Monoid( Last( .. )
                  , mempty
                  , (<>) )

import Graphics.Svg.Types
                   ( HasDrawAttributes( .. )
                   , Texture( ColorRef )
                   , Document( .. )
                   , drawAttr )
import Graphics.Svg( cssRulesOfText )

import Codec.Picture( PixelRGBA8( PixelRGBA8 ) )
import qualified Graphics.Svg.Types as Svg
import qualified Data.Set as S
import qualified Data.Text as T
import Text.Printf
import Linear( V2( .. )
             , (^+^)
             , (^-^)
             , (^*)
             , perp
             , normalize
             )
import Control.Lens( zoom, (.=), (%=) )

import Text.AsciiDiagram.Geometry
import Text.AsciiDiagram.DiagramCleaner

import Debug.Trace
{-import Text.Groom-}

data GridSize = GridSize
  { _gridCellWidth        :: !Float
  , _gridCellHeight       :: !Float
  , _gridShapeContraction :: !Float
  }
  deriving (Eq, Show)


toSvg :: GridSize -> Point -> Svg.RPoint
toSvg s (V2 x y) =
  V2 (_gridCellWidth s * fromIntegral (x + 1))
     (_gridCellHeight s * fromIntegral (y + 1))


setDashingInformation :: (Svg.WithDrawAttributes a) => a -> a
setDashingInformation = execState . zoom drawAttr $ do
  attrClass %= Last . adding . getLast
    where adding Nothing = Just "dashed_elem" 
          adding (Just v) = Just $ "dashed_elem " <> v


isShapeDashed :: Shape -> Bool
isShapeDashed = any isDashed . shapeElements where
  isDashed (ShapeAnchor _ _) = False
  isDashed (ShapeSegment Segment { _segDraw = SegmentSolid }) = False
  isDashed (ShapeSegment Segment { _segDraw = SegmentDashed }) = True

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


rollToSegment :: Shape -> Shape
rollToSegment shape | not $ shapeIsClosed shape = shape
rollToSegment shape = shape { shapeElements = segments ++ anchorPrefix } where
  (anchorPrefix, segments) = span isAnchor $ shapeElements shape

  isAnchor (ShapeSegment _) = False
  isAnchor (ShapeAnchor _ _) = True


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


straightCorner :: GridSize -> Bool -> Maybe Point -> Point -> Maybe Point
               -> Svg.Path
straightCorner gscale True (Just before) p (Just after) =
    lineTo $ anchorCorrection gscale before p after ^+^ toSvg gscale p
straightCorner gscale True (Just before) p _ =
    lineTo $ correctionVectorOf gscale before p ^+^ toSvg gscale p
straightCorner gscale _ _ p _ = lineTo $ toSvg gscale p



curveCorner :: GridSize -> Maybe Point -> Point -> Maybe Point -> Svg.Path
curveCorner gscale _ p (Just after) =
    smoothCurveTo (toS p) $ toS after ^+^ correction
  where correction = correctionVectorOf gscale p after
        toS = toSvg gscale
curveCorner gscale (Just before) p Nothing =
    smoothCurveTo (toS p) $ toS p ^+^ vec
  where vec = correctionVectorOf gscale before p
        toS = toSvg gscale
curveCorner gscale _ p _ = lineTo $ toSvg gscale p


roundedCorner :: GridSize -> Point -> Point -> Maybe Point -> Svg.Path
roundedCorner gscale p1 p2 (Just lastPoint) =
    Svg.CurveTo Svg.OriginAbsolute [(toS p1, toS p2, toS lastPoint ^+^ vec)]
  where toS = toSvg gscale
        vec = correctionVectorOf gscale p2 lastPoint
roundedCorner gscale p1 p2 after =
    curveCorner gscale (Just p1) p2 after


shapeToTree :: GridSize -> Shape -> Svg.Tree
shapeToTree gscale shape = 
{-trace (printf "TOTRANSFER ===============\n%s\nELEMS==============\n%s"-}
                                    {-(groom shape)-}
                                    {-(groom shapeElems)) $-}
    dashingSet . Svg.Path $ Svg.PathPrim mempty pathCommands where
  toS = toSvg gscale

  dashingSet
    | isShapeDashed shape = setDashingInformation 
    | otherwise = id

  shapeElems = associateNextPoint (shapeIsClosed shape)
             . reorderShapePoints
             $ rollToSegment shape

  isClosed = shapeIsClosed shape

  pathCommands =
    moveTo (startPoint gscale shapeElems)
        : toPath shapeElems ++ shapeClosing shape

  toPath [] = []
  toPath ((before, ShapeSegment seg, _):rest) =
      lineTo (vc ^+^ toS (_segEnd seg)) : toPath rest
    where vc = segmentCorrectionVector gscale before seg
  toPath ((_, ShapeAnchor p1 AnchorFirstDiag, _)
         :(_, ShapeAnchor p2 AnchorSecondDiag, after)
         :rest) = roundedCorner gscale p1 p2 after : toPath rest
  toPath ((_, ShapeAnchor p1 AnchorSecondDiag, _)
         :(_, ShapeAnchor p2 AnchorFirstDiag, after)
         :rest) = roundedCorner gscale p1 p2 after : toPath rest
  toPath ((before, ShapeAnchor p a, after):rest) = anchorJoin : toPath rest
    where
      anchorJoin = case a of
        AnchorPoint -> straightCorner gscale isClosed before p after
        AnchorMulti -> straightCorner gscale isClosed before p after
        AnchorFirstDiag -> curveCorner gscale before p after
        AnchorSecondDiag -> curveCorner gscale before p after

        AnchorArrowUp -> straightCorner gscale isClosed before p after
        AnchorArrowDown -> straightCorner gscale isClosed before p after
        AnchorArrowLeft -> straightCorner gscale isClosed before p after
        AnchorArrowRight -> straightCorner gscale isClosed before p after


textToTree :: GridSize -> TextZone -> Svg.Tree
textToTree gscale zone = Svg.TextArea Nothing txt
  where
    correction =
        V2 (negate $ _gridCellWidth gscale)
           (_gridCellHeight gscale) ^* 0.5
    V2 x y = toSvg gscale (_textZoneOrigin zone) ^+^ correction
    txt = Svg.textAt (Svg.Num x, Svg.Num y) $ _textZoneContent zone

defaultCss :: Float -> T.Text
defaultCss textSize = T.pack $ printf
  ("\n" <>
   "text { font-family: Consolas, monospace; font-size: %dpx }\n" <>
   ".dashed_elem { stroke-dasharray: 5 }\n"
  )
  (floor textSize :: Int)

svgOfDiagram :: Diagram -> Svg.Document
svgOfDiagram diagram = Document
  { _viewBox = Nothing
  , _width =
      toSvgSize _gridCellWidth $ _diagramCellWidth diagram + 1
  , _height =
      toSvgSize _gridCellHeight $ _diagramCellHeight diagram + 1
  , _elements = closedSvg ++ lineSvg ++ textSvg
  , _definitions = mempty
  , _description = ""
  , _styleRules =
      (\a -> trace (show a) a) $ cssRulesOfText . defaultCss $ _gridCellHeight scale
  }
  where
    (closed, opened) = S.partition shapeIsClosed shapes

    shapes = _diagramShapes diagram

    closedSvg =
        applyDefaultShapeDrawAttr . shapeToTree scale <$> filter isShapePossible
                                                            (S.toList closed)
    lineSvg = 
        applyDefaultLineDrawAttr . shapeToTree strokeScale <$> S.toList opened

    toSvgSize accessor var =
        Just . Svg.Num $ fromIntegral var * accessor scale + 5

    textSvg = textToTree scale <$> _diagramTexts diagram

    strokeScale = scale { _gridShapeContraction = 0 }
    scale = GridSize
      { _gridCellWidth = 10
      , _gridCellHeight = 14
      , _gridShapeContraction = 1.5
      }


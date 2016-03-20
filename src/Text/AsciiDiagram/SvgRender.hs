{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.AsciiDiagram.SvgRender( GridSize( .. )
                                  , defaultGridSize
                                  , svgOfDiagram
                                  , svgOfDiagramAtSize
                                  ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( mempty )
import Control.Applicative( (<$>) )
#endif

import Control.Monad.State.Strict( execState )
import Data.Monoid( (<>) )

import Graphics.Svg.Types
                   ( HasDrawAttributes( .. )
                   , Document( .. )
                   , drawAttr )
import Graphics.Svg( cssRulesOfText )

import Codec.Picture( PixelRGBA8( PixelRGBA8 ) )
import qualified Graphics.Svg.Types as Svg
import qualified Graphics.Svg.CssTypes as Css
import qualified Data.Map as M
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
import Control.Lens( zoom, (^.), (.=), (%=), (%~), (&) )

import Text.AsciiDiagram.Geometry
import Text.AsciiDiagram.DiagramCleaner
import Text.AsciiDiagram.BoundingBoxEstimation

{-import Debug.Trace-}
{-import Text.Groom-}

-- | Simple type describing the grid size used during render.
data GridSize = GridSize
  { _gridCellWidth        :: !Float -- ^ Width of a cell (in pixel)
  , _gridCellHeight       :: !Float -- ^ Height of a cell (in pixel)

    -- | Coefficient used to space adjacent shapes, set to 0
    -- if you want to remove space between them.
  , _gridShapeContraction :: !Float
  }
  deriving (Eq, Show)

-- | Default grid size used in the simple render functions
defaultGridSize :: GridSize
defaultGridSize = GridSize
  { _gridCellWidth = 10
  , _gridCellHeight = 14
  , _gridShapeContraction = 1.5
  }

toSvg :: GridSize -> Point -> Svg.RPoint
toSvg s (V2 x y) =
  V2 (realToFrac $ _gridCellWidth s * fromIntegral (x + 1))
     (realToFrac $ _gridCellHeight s * fromIntegral (y + 1))


setDashingInformation :: (Svg.WithDrawAttributes a) => a -> a
setDashingInformation = execState $ do
  drawAttr . attrClass %= ("dashed_elem":)

isShapeDashed :: Shape -> Bool
isShapeDashed = any isDashed . shapeElements where
  isDashed (ShapeAnchor _ _) = False
  isDashed (ShapeSegment Segment { _segDraw = SegmentSolid }) = False
  isDashed (ShapeSegment Segment { _segDraw = SegmentDashed }) = True

applyDefaultShapeDrawAttr :: (Svg.WithDrawAttributes a) => a -> a
applyDefaultShapeDrawAttr = execState . zoom drawAttr $ do
  attrClass %= ("filled_shape":) 

applyLineArrowDrawAttr :: (Svg.WithDrawAttributes a) => a -> a
applyLineArrowDrawAttr = execState . zoom drawAttr $ do
  attrClass %= ("arrow_head":)

applyBulletDrawAttr :: (Svg.WithDrawAttributes a) => a -> a
applyBulletDrawAttr = execState . zoom drawAttr $ do
  attrClass %= ("bullet":)

cleanupUseAttributes ::  (Svg.WithDrawAttributes a) => a -> a
cleanupUseAttributes = execState . zoom drawAttr $ do
  attrClass .= []

applyDefaultLineDrawAttr :: (Svg.WithDrawAttributes a) => a -> a
applyDefaultLineDrawAttr = execState . zoom drawAttr $ do
  attrClass %= ("line_element":)


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
    (Just before, ShapeAnchor p _, Just after):_ -> toS p ^+^ combined
        where v1 = realToFrac <$> correctionVector before p
              v2 = realToFrac <$> correctionVector p after
              combined | v1 == v2 = v1
                       | otherwise = v1 ^+^ v2
    (before, ShapeSegment seg, _):_ -> pp ^+^ vc where
       vc = segmentCorrectionVector gscale before seg
       pp = toS $ _segStart seg
    (_, ShapeAnchor p _, _):_ -> toS p
  where
    correctionVector = correctionVectorOf gscale
    toS = toSvg gscale


anchorCorrection :: GridSize -> Point -> Point -> Point
                 -> Svg.RPoint
anchorCorrection scale before p after
  | v1 == v2 = realToFrac <$> v1
  | otherwise = v1 ^+^ v2
  where v1 = realToFrac <$> correctionVectorOf scale before p
        v2 = realToFrac <$> correctionVectorOf scale p after


moveTo, lineTo :: Svg.RPoint -> Svg.PathCommand
moveTo p = Svg.MoveTo Svg.OriginAbsolute [p]
lineTo p = Svg.LineTo Svg.OriginAbsolute [p]


smoothCurveTo :: Svg.RPoint -> Svg.RPoint -> Svg.PathCommand
smoothCurveTo p1 p =
  Svg.SmoothCurveTo Svg.OriginAbsolute [(p1, p)]


shapeClosing :: Shape -> [Svg.PathCommand]
shapeClosing Shape { shapeIsClosed = True } = [Svg.EndPath]
shapeClosing _ = []


segmentCorrectionVector :: GridSize -> Maybe Point -> Segment -> Svg.RPoint
segmentCorrectionVector gscale before seg | _segStart seg == _segEnd seg =
  realToFrac <$> case (before, _segKind seg) of
    (Just v1, _) -> correctionVectorOf gscale v1 (_segEnd seg)
    (Nothing, SegmentHorizontal) -> V2 0 $ _gridShapeContraction gscale
    (Nothing, SegmentVertical) -> V2 (negate $ _gridShapeContraction gscale) 0
segmentCorrectionVector gscale _ seg =
    realToFrac <$> correctionVectorOf gscale (_segStart seg) (_segEnd seg)


straightCorner :: GridSize -> Bool -> Maybe Point -> Point -> Maybe Point
               -> ([Svg.PathCommand], [Svg.Tree])
straightCorner gscale isBullet pBefore p pAfter
    | isBullet = ([lineTo finalPoint], [renderBullet gscale finalPoint])
    | otherwise =  ([lineTo finalPoint], [])
  where
    pSvg = toSvg gscale p
    finalPoint = case (pBefore, pAfter) of
       (Just before, Just after) ->
          anchorCorrection gscale before p after ^+^ pSvg
       (Just before, _) -> 
          (realToFrac <$> correctionVectorOf gscale before p) ^+^ pSvg
       _ -> pSvg

curveCorner :: GridSize -> Maybe Point -> Point -> Maybe Point -> Svg.PathCommand
curveCorner gscale _ p (Just after) =
    smoothCurveTo (toS p) $ toS after ^+^ correction
  where correction = realToFrac <$> correctionVectorOf gscale p after
        toS = toSvg gscale
curveCorner gscale (Just before) p Nothing =
    smoothCurveTo (toS p) $ toS p ^+^ vec
  where vec = realToFrac <$> correctionVectorOf gscale before p
        toS = toSvg gscale
curveCorner gscale _ p _ = lineTo $ toSvg gscale p


roundedCorner :: GridSize -> Point -> Point -> Maybe Point -> Svg.PathCommand
roundedCorner gscale p1 p2 (Just lastPoint) =
    Svg.CurveTo Svg.OriginAbsolute [(toS p1, toS p2, toS lastPoint ^+^ vec)]
  where toS = toSvg gscale
        vec = realToFrac <$> correctionVectorOf gscale p2 lastPoint
roundedCorner gscale p1 p2 after =
    curveCorner gscale (Just p1) p2 after

toPathRooted :: [Svg.RPoint] -> GridSize -> Point -> Svg.Tree
toPathRooted pts gscale p =
    applyLineArrowDrawAttr . Svg.PathTree $ Svg.Path mempty pathCommands
  where
    pt = fromIntegral <$> p
    sizes =
      realToFrac <$> V2 (_gridCellWidth gscale) (_gridCellHeight gscale)

    toGrid pp = lineTo $ (pt ^+^ pp) * sizes
    pathCommands = case pts of
      [] -> []
      x:xs -> moveTo ((pt ^+^ x) * sizes)
                : fmap toGrid xs ++ [Svg.EndPath]

toRightArrow :: GridSize -> Point -> Svg.Tree
toRightArrow =
  toPathRooted [ V2 1 0.5
               , V2 2 1
               , V2 1 1.5
               ]

toLeftArrow :: GridSize -> Point -> Svg.Tree
toLeftArrow =
  toPathRooted [ V2 1 0.5
               , V2 0 1
               , V2 1 1.5
               ]

toTopArrow :: GridSize -> Point -> Svg.Tree
toTopArrow =
  toPathRooted [ V2 0.5 1
               , V2 1.5 1
               , V2 1   0
               ]

toBottomArrow :: GridSize -> Point -> Svg.Tree
toBottomArrow =
  toPathRooted [ V2 0.5 1
               , V2 1.5 1
               , V2 1   2
               ]

renderBullet :: GridSize -> Svg.RPoint -> Svg.Tree
renderBullet gscale (V2 x y) = applyBulletDrawAttr $ Svg.CircleTree Svg.defaultSvg
  { Svg._circleCenter = (Svg.Num $ realToFrac x, Svg.Num $ realToFrac y)
  , Svg._circleRadius = Svg.Num . realToFrac $ halfWidth - 2
  }
  where halfWidth = _gridCellWidth gscale / 2

dashingSet :: (Svg.WithDrawAttributes a) => Shape -> a -> a
dashingSet shape
  | isShapeDashed shape = setDashingInformation
  | otherwise = id

classSet :: (Svg.WithDrawAttributes a) => Shape -> a -> a
classSet shape e =
  e & drawAttr . attrClass %~ (++ S.toList (shapeTags shape))

shapeToTree :: GridSize -> Shape -> Svg.Tree
shapeToTree gscale shape@Shape
    { shapeIsClosed = True
    , shapeElements =
        [ ShapeSegment _
        , ShapeAnchor p0 AnchorMulti
        , ShapeSegment _
        , ShapeAnchor p1 AnchorMulti
        , ShapeSegment _
        , ShapeAnchor p2 AnchorMulti
        , ShapeSegment _
        , ShapeAnchor p3 AnchorMulti ]
    } = classSet shape
      . dashingSet shape
      . Svg.RectangleTree
      $ Svg.defaultSvg
          { Svg._rectWidth = Svg.Num sWidth
          , Svg._rectHeight = Svg.Num sHeight
          , Svg._rectUpperLeftCorner = (Svg.Num px, Svg.Num py) }
  where
    pts = [p0, p1, p2, p3]
    mini = minimum pts
    maxi = maximum pts
    contraction = _gridShapeContraction gscale
    contractionVector = realToFrac <$> V2 contraction contraction

    maxiPoint = toSvg gscale maxi ^-^ contractionVector
    pt@(V2 px py) = toSvg gscale mini ^+^ contractionVector 
    V2 sWidth sHeight = maxiPoint ^-^ pt



shapeToTree gscale shape =
  case concat arrows of
    [] -> svgPath
    lst -> Svg.GroupTree $ Svg.defaultSvg { Svg._groupChildren = svgPath : lst }
  where
    toS = toSvg gscale
    shapeElems = associateNextPoint (shapeIsClosed shape)
               . reorderShapePoints
               $ rollToSegment shape

    svgPath = classSet shape . dashingSet shape . Svg.PathTree
            $ Svg.Path mempty pathCommands

    pathCommands =
      moveTo (startPoint gscale shapeElems)
          : concat pathes ++ shapeClosing shape
    (pathes, arrows) = unzip $ toPath shapeElems

    toPath [] = []
    toPath ((before, ShapeSegment seg, Just _):rest) =
        ([lineTo (vc ^+^ toS (_segEnd seg))], []) : toPath rest
      where vc = segmentCorrectionVector gscale before seg
    toPath ((before, ShapeSegment seg, Nothing):rest) =
        ([lineTo (vc ^+^ toS (_segEnd seg))], []) : toPath rest
      where vc = segmentCorrectionVector gscale before seg'
            extension = signum <$> (_segEnd seg ^-^ _segStart seg)
            seg' = seg { _segEnd = _segEnd seg ^+^ extension }
    toPath ((_, ShapeAnchor p1 AnchorFirstDiag, _)
           :(_, ShapeAnchor p2 AnchorSecondDiag, after)
           :rest) = ([roundedCorner gscale p1 p2 after], []) : toPath rest
    toPath ((_, ShapeAnchor p1 AnchorSecondDiag, _)
           :(_, ShapeAnchor p2 AnchorFirstDiag, after)
           :rest) = ([roundedCorner gscale p1 p2 after], []) : toPath rest
    toPath ((before, ShapeAnchor p a, after):rest) = anchorJoin : toPath rest
      where
        anchorJoin = case a of
          AnchorPoint -> straightCorner gscale False before p after
          AnchorMulti -> straightCorner gscale False before p after
          AnchorBullet -> straightCorner gscale True before p after
            
          AnchorFirstDiag -> ([curveCorner gscale before p after], [])
          AnchorSecondDiag -> ([curveCorner gscale before p after], [])

          AnchorArrowUp -> ([lineTo $ toS p], [toTopArrow gscale p])
          AnchorArrowDown -> ([lineTo $ toS p], [toBottomArrow gscale p])
          AnchorArrowLeft -> ([lineTo $ toS p], [toLeftArrow gscale p])
          AnchorArrowRight -> ([lineTo $ toS p], [toRightArrow gscale p])


textToTree :: GridSize -> TextZone -> Svg.Tree
textToTree gscale zone = Svg.TextTree Nothing txt
  where
    correction = realToFrac <$>
        V2 (negate $ _gridCellWidth gscale)
           (_gridCellHeight gscale) ^* 0.5
    V2 x y = toSvg gscale (_textZoneOrigin zone) ^+^ correction
    txt = Svg.textAt (Svg.Num (x+0.5), Svg.Num (y+0.5)) $ _textZoneContent zone

defaultCss :: Float -> T.Text
defaultCss textSize = T.pack $ printf
  ("\n" <>
   "text { font-family: Consolas, \"DejaVu Sans Mono\", monospace; font-size: %dpx }\n" <>
   ".dashed_elem { stroke-dasharray: 4, 3 }\n" <>
   ".filled_shape { fill: url(#shape_light); stroke: black; stroke-width: 1px; }\n" <>
   ".line_element { fill: none; stroke: black; stroke-width: 1px; }\n" <>
   ".bullet { stroke-width: 1px; fill: white; stroke: black }\n" <>
   ".arrow_head { fill: black; stroke: none; }\n"
  )
  (2 + floor textSize :: Int)

lightShapeGradient :: Svg.Element
lightShapeGradient = Svg.ElementLinearGradient $
    Svg.defaultSvg
        { Svg._linearGradientStart = (Svg.Percent 0, Svg.Percent 0)
        , Svg._linearGradientStop =  (Svg.Percent 0, Svg.Percent 1)
        , Svg._linearGradientStops =
            [ Svg.GradientStop 0 $ PixelRGBA8 245 245 245 255
            , Svg.GradientStop 1 $ PixelRGBA8 216 216 216 255
            ]
        }

-- | Transform an Ascii diagram to a SVG document which
-- can be saved or converted to an image.
svgOfDiagram :: Diagram -> Svg.Document
svgOfDiagram = svgOfDiagramAtSize defaultGridSize where

svgOfShape :: GridSize -> Shape -> Svg.Tree
svgOfShape scale shape
  | shapeIsClosed shape =
      applyDefaultShapeDrawAttr $ shapeToTree scale shape
  | otherwise =
      applyDefaultLineDrawAttr $ shapeToTree strokeScale shape
  where
    strokeScale = scale { _gridShapeContraction = 0 }


svgOfElement :: GridSize -> Element -> Svg.Tree
svgOfElement scale (ElemText txt) = textToTree scale txt
svgOfElement scale (ElemShape shape) = 
  case shapeChildren shape of
    [] -> svgOfShape scale shape
    _:_ -> Svg.GroupTree $ group
  where
    thisShape = svgOfShape scale shape
    group = Svg.defaultSvg
      { Svg._groupDrawAttributes =
          mempty { Svg._attrClass = S.toList $ shapeTags shape }
      , Svg._groupChildren =
          thisShape : fmap (svgOfElement scale) (shapeChildren shape)
      , Svg._groupViewBox = Nothing
      }

shapeRewriter :: [Css.CssRule] -> Svg.Tree -> Svg.Tree
shapeRewriter rules = Svg.zipTree go where
  go [] = Svg.None
  go ([]:_) = Svg.None
  go context@((t:_):_) = case reverse shapeDeclarations of
      [] -> t
      (([Css.CssIdent i]:_): _) -> Svg.UseTree (useInfo i) Nothing
      _ -> t
   where
     useInfo name = cleanupUseAttributes $ Svg.Use
        { Svg._useDrawAttributes = t ^. Svg.drawAttr
        , Svg._useBase = (Css.Num x, Css.Num y)
        , Svg._useName = T.unpack name
        , Svg._useWidth = Just (Css.Num w)
        , Svg._useHeight = Just (Css.Num h)
        }

     BoundingBox pMin@(V2 x y) pMax = boundingBoxOf t
     V2 w h = pMax ^-^ pMin

     shapeDeclarations = 
        [el | Css.CssDeclaration "shape" el <- Css.findMatchingDeclarations rules context]

defaultCircle :: Svg.Element
defaultCircle = Svg.ElementGeometry $ Svg.SymbolTree tree where
  tree = Svg.Symbol $ execState build Svg.defaultSvg

  build = do
    let circle = applyDefaultShapeDrawAttr $ Svg.defaultSvg
            { Svg._circleCenter = (Css.Num 25, Css.Num 25)
            , Svg._circleRadius = Css.Num 24.5
            }
    Svg.groupChildren .= [Svg.CircleTree circle]
    Svg.groupViewBox .= Just (0, 0, 50, 50)
    Svg.groupAspectRatio . Svg.aspectRatioAlign .= Svg.AlignNone

defaultDocument :: Svg.Element
defaultDocument = Svg.ElementGeometry $ Svg.SymbolTree tree where
  tree = Svg.Symbol $ execState build Svg.defaultSvg

  build = do
    let path = applyDefaultShapeDrawAttr $ Svg.defaultSvg
            { Svg._pathDefinition =
                [ Svg.MoveTo Svg.OriginRelative [V2 49.98 0.79, V2 (-49.166) 0, V2 0 45.402]
                , Svg.CurveTo Svg.OriginRelative [(V2 15.92 12.45, V2 30.40 (-13.44), V2 49.17 0)]
                , Svg.EndPath
                ]
            }
    Svg.groupChildren .= [Svg.PathTree path]
    Svg.groupViewBox .= Just (0, 0, 51, 51)
    Svg.groupAspectRatio . Svg.aspectRatioAlign .= Svg.AlignNone

-- | Transform an Ascii diagram to a SVG document which
-- can be saved or converted to an image, with a customizable
-- grid size.
svgOfDiagramAtSize :: GridSize -> Diagram -> Svg.Document
svgOfDiagramAtSize scale diagram = Document
  { _viewBox = Nothing
  , _width =
      toSvgSize _gridCellWidth $ _diagramCellWidth diagram + 1
  , _height =
      toSvgSize _gridCellHeight $ _diagramCellHeight diagram + 1
  , _elements =
      shapeRewriter customCssRules . svgOfElement scale <$> shapes
  , _definitions = M.fromList
        [ ("shape_light", lightShapeGradient)
        , ("circle", defaultCircle)
        , ("document", defaultDocument)
        ]
  , _description = ""
  , _styleRules = defaultCssRules ++ customCssRules
  , _documentLocation = ""
  }
  where
    defaultCssRules =
      cssRulesOfText . defaultCss $ _gridCellHeight scale

    customCssRules = 
      cssRulesOfText . T.unlines $ _diagramStyles diagram

    isDrawable (ElemText _) = True
    isDrawable (ElemShape shape) =
      not (shapeIsClosed shape) || isShapePossible shape
    shapes = filter isDrawable . S.toList $ _diagramElements diagram

    toSvgSize accessor var =
        Just . Svg.Num . realToFrac $ fromIntegral var * accessor scale + 5


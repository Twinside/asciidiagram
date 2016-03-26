{-# LANGUAGE OverloadedStrings #-}
module Text.AsciiDiagram.DefaultContext
    ( defaultCss
    , defaultCssRules
    , defaultDefinitions
    ) where

import Control.Monad.State.Strict( execState )
import Data.Monoid( (<>) )

import Graphics.Svg.Types ( HasDrawAttributes( .. ), drawAttr )
import Graphics.Svg( cssRulesOfText )

import Codec.Picture( PixelRGBA8( PixelRGBA8 ) )
import qualified Graphics.Svg.Types as Svg
import qualified Graphics.Svg.CssTypes as Css
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Printf
import Linear( V2( .. ) )
import Control.Lens( (.=), (.~), (&) )


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

defaultCssRules :: Float -> [Css.CssRule]
defaultCssRules = cssRulesOfText . defaultCss

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

toSymbol :: Double -> Double -> [Svg.Tree] -> Svg.Element
toSymbol width height geom = element where
  element = Svg.ElementGeometry $ Svg.SymbolTree tree
  tree = Svg.Symbol $ execState build Svg.defaultSvg

  build = do
    Svg.groupChildren .= geom
    Svg.groupViewBox .= Just (0, 0, width, height)
    Svg.groupAspectRatio . Svg.aspectRatioAlign .= Svg.AlignNone

asFilled :: (Svg.WithDrawAttributes a) => a -> a
asFilled el = el & drawAttr.attrClass .~ ["filled_shape"]

defaultCircle :: Svg.Element
defaultCircle = toSymbol 50 50 [Svg.CircleTree $ asFilled circle] where
  circle = Svg.defaultSvg
    { Svg._circleCenter = (Css.Num 25, Css.Num 25)
    , Svg._circleRadius = Css.Num 24.5
    }

h, v :: [Svg.Coord] -> Svg.PathCommand
h = Svg.HorizontalTo Svg.OriginRelative
v = Svg.VerticalTo Svg.OriginRelative

m :: [Svg.RPoint] -> Svg.PathCommand
m = Svg.MoveTo Svg.OriginRelative

c :: [(Svg.RPoint, Svg.RPoint, Svg.RPoint)] -> Svg.PathCommand
c = Svg.CurveTo Svg.OriginRelative

l :: [Svg.RPoint] -> Svg.PathCommand
l = Svg.LineTo Svg.OriginRelative

z :: Svg.PathCommand
z = Svg.EndPath

defaultDocument :: Svg.Element
defaultDocument = toSymbol 51 51 [Svg.PathTree $ asFilled path] where
  path = Svg.defaultSvg
    { Svg._pathDefinition =
        [ m [V2 49.98 0.79, V2 (-49.166) 0, V2 0 45.402]
        , c [(V2 15.92 12.45, V2 30.40 (-13.44), V2 49.17 0)]
        , z
        ]
    }

ioShape :: Svg.Element
ioShape = toSymbol 100 100 [Svg.PathTree $ asFilled path] where
  path = Svg.defaultSvg
    { Svg._pathDefinition =
        [ m [V2 19.207 1.6]
        , h [79.414]
        , l [V2 (-17.192) 97.297]
        , h [-79.414]
        , z
        ]
    }

storageShape :: Svg.Element
storageShape = toSymbol 80 80 geometry where
  geometry = Svg.PathTree . asFilled <$> [path1, path2]
  path1 = Svg.defaultSvg
    { Svg._pathDefinition =
        [ m [V2 4.8122 67.74]
        , v [-53.769]
        , c [ (V2 0.0 (-3.2991) , V2 16.067 (-5.9743), V2 35.845 (-5.9743))
            , (V2 19.78 0.0, V2 35.848 2.6752, V2 35.848 5.9743)
            ]
       , v [53.769]
       , c [(V2 0.0 3.2925, V2 (-16.068) 5.9743, V2 (-35.848) 5.9743)
           , (V2 (-19.779) 0.0, V2 (-35.845) (-2.6818), V2 (-35.845) (-5.9743))
           ]
        , z
        ]
    }
  path2 = Svg.defaultSvg
    { Svg._pathDefinition =
        [ m [ V2 4.8122 13.97, V2 4.779e-2 0.30672, V2 0.13807 0.30272
            , V2 0.22834 0.29872, V2 0.31464 0.29344, V2 0.40094 0.28944
            , V2 0.48325 0.2828, V2 0.56423 0.27744, V2 0.64256 0.2708
            , V2 0.71823 0.26416, V2 0.79258 0.25752, V2 0.86294 0.25096
            , V2 0.9333 0.2416, V2 0.99968 0.23496, V2 1.0647 0.22568
            , V2 1.1271 0.2164, V2 1.1869 0.2084, V2 1.2453 0.19784
            , V2 1.301 0.1872, V2 1.3555 0.17792, V2 1.4046 0.16728
            , V2 1.455 0.15536, V2 1.5002 0.14336, V2 1.5466 0.1328
            , V2 1.5865 0.11952, V2 1.6276 0.10752, V2 1.6648 9.42e-2
            , V2 1.7007 8.1e-2, V2 1.7338 6.64e-2, V2 1.7644 5.31e-2
            , V2 1.7923 3.72e-2, V2 1.8188 2.39e-2, V2 1.8427 8.0e-3
            ]
        , c [(V2 19.78 0.0, V2 35.848 (-2.6818), V2 35.848 (-5.9743))]
        ]
    }

defaultDefinitions :: M.Map String Svg.Element
defaultDefinitions = M.fromList
  [ ("shape_light", lightShapeGradient)
  , ("circle", defaultCircle)
  , ("document", defaultDocument)
  , ("io", ioShape)
  , ("storage", storageShape)
  ]


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

defaultCircle :: Svg.Element
defaultCircle = Svg.ElementGeometry $ Svg.SymbolTree tree where
  tree = Svg.Symbol $ execState build Svg.defaultSvg

  build = do
    let circle' = Svg.defaultSvg
            { Svg._circleCenter = (Css.Num 25, Css.Num 25)
            , Svg._circleRadius = Css.Num 24.5
            }
        circle = circle' & drawAttr.attrClass .~ ["filled_shape"]
        
    Svg.groupChildren .= [Svg.CircleTree circle]
    Svg.groupViewBox .= Just (0, 0, 50, 50)
    Svg.groupAspectRatio . Svg.aspectRatioAlign .= Svg.AlignNone

defaultDocument :: Svg.Element
defaultDocument = Svg.ElementGeometry $ Svg.SymbolTree tree where
  tree = Svg.Symbol $ execState build Svg.defaultSvg

  build = do
    let path' = Svg.defaultSvg
            { Svg._pathDefinition =
                [ Svg.MoveTo Svg.OriginRelative [V2 49.98 0.79, V2 (-49.166) 0, V2 0 45.402]
                , Svg.CurveTo Svg.OriginRelative [(V2 15.92 12.45, V2 30.40 (-13.44), V2 49.17 0)]
                , Svg.EndPath
                ]
            }
        path = path' & drawAttr.attrClass .~ ["filled_shape"]
    Svg.groupChildren .= [Svg.PathTree path]
    Svg.groupViewBox .= Just (0, 0, 51, 51)
    Svg.groupAspectRatio . Svg.aspectRatioAlign .= Svg.AlignNone

defaultDefinitions :: M.Map String Svg.Element
defaultDefinitions = M.fromList
  [ ("shape_light", lightShapeGradient)
  , ("circle", defaultCircle)
  , ("document", defaultDocument)
  ]


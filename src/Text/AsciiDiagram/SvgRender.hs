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
import Linear( V2( .. ) )
import Control.Lens( zoom, (.=) )

import Text.AsciiDiagram.Geometry

type GridScale = (Float, Float)

toSvg :: GridScale -> Point -> Svg.RPoint
toSvg (sx, sy) (V2 x y) =
  V2 (sx * fromIntegral x) (sy * fromIntegral y)

applyDefaultShapeDrawAttr :: (Svg.WithDrawAttributes a) => a -> a
applyDefaultShapeDrawAttr = execState . zoom drawAttr $ do
    fillColor .= toLC 200 200 200 255
    strokeColor .= toLC 0 0 0 255
    strokeWidth .= toL (Svg.Num 1)
  where
    toL = Last . Just
    toLC r g b a =
        toL . ColorRef $ PixelRGBA8 r g b a


shapeToTree :: GridScale -> Shape -> Svg.Tree
shapeToTree gscale shape =
    Svg.Path $ Svg.PathPrim mempty pathCommands where
  toS = toSvg gscale

  shapeElems = shapeElements shape

  close | shapeIsClosed shape = [Svg.EndPath]
        | otherwise = []
  
  startPoint = firstPointOfShape shapeElems

  moveTo p = Svg.MoveTo Svg.OriginAbsolute [toS p]
  lineTo p = Svg.LineTo Svg.OriginAbsolute [toS p]

  pathCommands =
    moveTo startPoint : fmap toPath shapeElems ++ close

  toPath (ShapeAnchor p _) = lineTo p
  toPath (ShapeSegment seg) = lineTo $ _segEnd seg

shapesToSvgDocument :: S.Set Shape -> Svg.Document
shapesToSvgDocument shapes = Document	 
  { _viewBox = Nothing
  , _width = Just $ Svg.Num 200
  , _height = Just $ Svg.Num 100
  , _elements =
      applyDefaultShapeDrawAttr . shapeToTree scale <$> S.toList shapes
  , _definitions = mempty
  , _description = ""
  , _styleText  = ""
  , _styleRules = []
  }
  where scale = (11, 11)


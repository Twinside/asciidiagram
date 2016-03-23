{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | This module gives access to the ascii diagram parser and
-- SVG renderer.
--
-- Ascii diagram, transform your ASCII art drawing to a nicer
-- representation
-- 
-- @
--                 \/---------+
-- +---------+     |         |
-- |  ASCII  +----\>| Diagram |
-- +---------+     |         |
-- |{flat}   |     +--+------\/
-- \\---*-----\/\<=======\/
-- ::: .flat { fill: #DDD; }
-- @
-- <<docimages/baseExample.svg>>
-- 
-- To render the diagram as a PNG file, you have to use the
-- library rasterific-svg and JuicyPixels.
--
-- As a sample usage, to save a diagram to png, you can use
-- the following snippet.
--
-- > import Codec.Picture( writePng )
-- > import Text.AsciiDiagram( imageOfDiagram )
-- > import Graphics.Rasterific.Svg( loadCreateFontCache )
-- >
-- > saveDiagramToFile :: FilePath -> Diagram -> IO ()
-- > saveDiagramToFile path diag = do
-- >   cache <- loadCreateFontCache "asciidiagram-fonty-fontcache"
-- >   imageOfDiagram cache 96 diag
-- >   writePng path img
--
module Text.AsciiDiagram
  ( 
    -- $introDoc
    -- * Diagram format

    -- ** Lines
    -- $linesdoc

    -- ** Shapes
    -- $shapesdoc

    -- ** Bullets
    -- $bulletdoc

    -- ** Styles
    -- $styledoc

    -- * Usage example
    -- $example

    -- * Functions
    svgOfDiagram
  , parseAsciiDiagram
  , saveAsciiDiagramAsSvg
  , imageOfDiagram
  , pdfOfDiagram

   -- * Customized rendering
  , svgOfDiagramAtSize 
  , GridSize( .. )
  , defaultGridSize
  , saveAsciiDiagramAsSvgAtSize
  , imageOfDiagramAtSize
  , pdfOfDiagramAtSize

    -- * Library
  , defaultLibrary

    -- * Document description
  , Diagram( .. ) 
  , TextZone( .. )
  , Shape( .. )
  , ShapeElement( .. )
  , Anchor( .. )
  , Segment( .. )
  , SegmentKind( .. )
  , SegmentDraw( .. )
  , Point
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>) )
#endif

import Data.Monoid( (<>))
import Control.Monad( forM_ )
import Control.Monad.ST( runST )
import Data.Function( on )
import Data.List( partition, sortBy )
import qualified Data.ByteString.Lazy as LB
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import Linear( V2( V2 ) )

import Text.AsciiDiagram.Parser
import Text.AsciiDiagram.Reconstructor
import Text.AsciiDiagram.SvgRender
import Text.AsciiDiagram.Geometry
import Text.AsciiDiagram.DiagramCleaner

import Codec.Picture( Image, PixelRGBA8 )
import Graphics.Text.TrueType( FontCache )
import Graphics.Svg( saveXmlFile )
import Graphics.Rasterific.Svg( renderSvgDocument, pdfOfSvgDocument )

{-import Debug.Trace-}
{-import Text.Groom-}
{-import Text.Printf-}

data CharBoard = CharBoard
  { _boardWidth  :: !Int
  , _boardHeight :: !Int
  , _boardData   :: !(VU.Vector Char)
  }
  deriving (Eq, Show)

textOfCharBoard :: CharBoard -> [T.Text]
textOfCharBoard board = fetch <$> zip [0 .. h - 1] [0, w..] where
  w = _boardWidth board
  h = _boardHeight board
  charData = _boardData board

  fetch (_, startIdx) =
      T.pack . VU.toList . VU.take w $ VU.drop startIdx charData

charBoardOfText :: [T.Text] -> CharBoard
charBoardOfText textLines = CharBoard
  { _boardWidth  = twidth
  , _boardHeight = theight
  , _boardData   = charData
  }
  where
    twidth = maximum $ fmap T.length textLines
    theight = length textLines
 
    lineIndices = zip [0, twidth ..] textLines

    charData = runST $ do
      emptyBoard <- VUM.replicate (twidth * theight) ' '

      forM_ lineIndices $ \(lineIndex, l) -> do
        let chars = zip [lineIndex, lineIndex + 1 ..] $ T.unpack l
        forM_ chars $ \(idx, c) -> do
          VUM.unsafeWrite emptyBoard idx c

      VU.unsafeFreeze emptyBoard


pointsOfShape :: F.Foldable f => f Shape -> [Point]
pointsOfShape = F.concatMap (F.concatMap go . shapeElements) where
  go (ShapeAnchor p _) = [p]
  go (ShapeSegment Segment { _segStart = V2 sx sy, _segEnd = V2 ex ey })
    | sx == ex && sy >= ey = [V2 sx yy | yy <- [ey .. sy]]
    | sx == ex             = [V2 sx yy | yy <- [sy .. ey]]
    | sy == ey && sx >= ex = [V2 xx sy | xx <- [ex .. sx]]
    | sy == ey = [V2 xx sy | xx <- [sx .. ex]]
    | otherwise            = []

cleanLines :: [Int] -> CharBoard -> CharBoard
cleanLines idxs board = board { _boardData = _boardData board VU.// toSet }
  where
    xMax = _boardWidth board - 1
    toSet = [(lineIndex + column, ' ')
                          | lineNum <- idxs
                          , let lineIndex = lineNum * _boardWidth board
                          , column <- [0 .. xMax]
                          ]

cleanupShapes :: (F.Foldable f) => f Shape -> CharBoard -> CharBoard
cleanupShapes shapes board = board { _boardData = _boardData board VU.// toSet }
  where
    toSet = [(x + y * _boardWidth board, ' ') | V2 x y <- pointsOfShape shapes]


pointComp :: Point -> Point -> Ordering
pointComp (V2 x1 y1) (V2 x2 y2) = case compare y1 y2 of
  EQ -> compare x1 x2
  a -> a

featuresOfClosedShape :: Shape -> [Point]
featuresOfClosedShape = F.fold . go . shapeElements where
   go [] = []
   -- If we got something like +----+ we skip the first anchor and
   -- the segment.
   go ( ShapeAnchor (V2 _ ay1) _
      : ShapeSegment Segment { _segStart = V2 _ sy, _segEnd = V2 _ ey }
      : rest@(ShapeAnchor (V2 _ ay2) _ : _)
      )
       | ay1 == ay2 && sy == ey && ay1 == sy = go rest
   go (ShapeAnchor p _: rest) = [p] : go rest
   go (ShapeSegment Segment { _segStart = V2 sx sy, _segEnd = V2 ex ey } : rest)
     | sx == ex && sy >= ey = [V2 sx yy | yy <- [ey .. sy]] : after
     | sx == ex             = [V2 sx yy | yy <- [sy .. ey]] : after
     | otherwise           = after
       where after = go rest

rangesOfOpenedShape :: Shape -> [(Point, Point)]
rangesOfOpenedShape s = fmap dup . sortBy pointComp $ pointsOfShape [s]
  where dup a = (a, a)

rangesOfClosedShape :: Shape -> [(Point, Point)]
rangesOfClosedShape shape = pairAssoc sortedPoints
  where
   pairAssoc  [] = []
   pairAssoc [_] = []
   pairAssoc (p1@(V2 _ y1):lst@(p2@(V2 _ y2):rest))
      | y1 == y2 = (p1, p2) : pairAssoc rest
      | otherwise = pairAssoc lst

   sortedPoints = sortBy pointComp $ featuresOfClosedShape shape


class RangeDecomposable a where
  rangesOf :: a -> [(Point, Point)]

instance RangeDecomposable Shape where
  rangesOf s
     | shapeIsClosed s = rangesOfClosedShape s
     | otherwise = rangesOfOpenedShape s

instance RangeDecomposable TextZone where
  rangesOf txt = [(orig, V2 (x + txtLength) y)] where
    orig@(V2 x y) = _textZoneOrigin txt
    txtLength = T.length $ _textZoneContent txt


contains :: (RangeDecomposable a, RangeDecomposable b) => a -> b -> Bool
contains sa sb = go (rangesOf sa) (rangesOf sb) where
  go _      []    = True
  go []     (_:_) = False
  go ((V2 _ ya, _):rest1) rest2@((V2 _ yb, _):_)
    -- A part of second shape is before potentially englobing shpae
    -- so sa can't contain sb
    | ya > yb = False   
    -- sa may be bigger, just skip
    | ya < yb = go rest1 rest2
  -- here ya == yb
  go sal@((V2 xa1 _, V2 xa2 _):rest1)
     sar@((V2 xb1 _, V2 xb2 _):rest2)
    -- sb is before any range of sa, so we must have
    -- missed something, sa not containing sb
    | xb1 < xa1 = False
    -- sb is in the range bounds
    | xa1 <= xb1 && xb2 <= xa2 = go sal rest2
    -- Maybe sa has another range on the same line
    | xb1 > xa2 = go rest1 sar
    | otherwise = False
  
areaOfShape :: Shape -> Int
areaOfShape = F.sum . fmap dist . rangesOfClosedShape
  where
    -- we can use manathan distance here
    dist (V2 x1 y1, V2 x2 y2) = abs (x1 - x2) + abs (y1 - y2)

sortByArea :: [Shape] -> [Shape]
sortByArea shapes = sorted <> opened
  where
    (closed, opened) = partition shapeIsClosed shapes
    sorted =
        fmap snd . reverse $ sortBy (compare `on` fst) [(areaOfShape s, s) | s <- closed]

hierarchise :: [Shape] -> [TextZone] -> [TextZone] -> [Element]
hierarchise shapes allTexts = finalize . go areaSortedShapes allTexts where
  areaSortedShapes = sortByArea shapes

  finalize (topShapes, topText, _topTags) =
    (ElemShape <$> topShapes) <> (ElemText <$> topText)

  go [] texts tags = ([], texts, tags)
  go (x:xs) texts tags | not (shapeIsClosed x) = (x:outShapes, outTexts, outTags)
    where
      (outShapes, outTexts, outTags) = go xs texts tags
  go (x:xs) texts tags = (newShape : restShapes, restText, restTags)
    where
      (shapeInShape, shapeOutShape) = partition (x `contains`) xs
      (textInShape, textOutShape) = partition (x `contains`) texts
      (tagsInShape, tagsOutShape) = partition (x `contains`) tags

      (innerShapes, innerText, innerTags) = go shapeInShape textInShape tagsInShape
      (restShapes, restText, restTags) = go shapeOutShape textOutShape tagsOutShape

      newShape = x
        { shapeChildren =
            (ElemShape <$> innerShapes) <> (ElemText <$> innerText)
        , shapeTags = S.fromList $ _textZoneContent <$> innerTags
        }

-- | Analyze an ascii diagram and extract all it's features.
parseAsciiDiagram :: T.Text -> Diagram
parseAsciiDiagram content = Diagram
    { _diagramElements = S.fromList allElements
    , _diagramCellWidth = maximum $ fmap T.length textLines
    , _diagramCellHeight = length textLines - length styleLines
    , _diagramStyles = reverse styleLines
    }
  where
    textLines = T.lines content
    allElements = hierarchise (S.toList validShapes) nonEmptyZones tags

    nonEmptyZones = [t | t <- zones, not . T.null $ _textZoneContent t]
    (tags, zones) = detectTagFromTextZone $ extractTextZones shapeCleanedText 
    (styleLineNumber, styleLines) = unzip $ styleLine parsed

    shapeCleanedText =
      textOfCharBoard . cleanLines styleLineNumber
                      . cleanupShapes validShapes
                      $ charBoardOfText textLines
    
    parsed = parseTextLines textLines
    reconstructed =
      reconstruct (anchorMap parsed) $ segmentSet parsed
    validShapes = S.filter isShapePossible reconstructed

-- | Helper function helping you save a diagram as
-- a SVG file on disk.
saveAsciiDiagramAsSvg :: FilePath -> Diagram -> IO ()
saveAsciiDiagramAsSvg fileName diagram =
  saveXmlFile fileName $ svgOfDiagram diagram

-- | Helper function helping you save a diagram as
-- a SVG file on disk with a customized grid size.
saveAsciiDiagramAsSvgAtSize :: FilePath -> GridSize -> Diagram -> IO ()
saveAsciiDiagramAsSvgAtSize fileName gridSize =
  saveXmlFile fileName . svgOfDiagramAtSize gridSize

-- | Render a Diagram as an image. The Dpi
-- is 96. The IO dependency is there to allow loading of the
-- font files used in the document.
imageOfDiagram :: FontCache -> Diagram -> IO (Image PixelRGBA8)
imageOfDiagram cache = 
  fmap fst . renderSvgDocument cache Nothing 96 . svgOfDiagram

-- | Render a Diagram as an image with a custom grid size. The Dpi
-- is 96. The IO dependency is there to allow loading of the
-- font files used in the document.
imageOfDiagramAtSize :: FontCache -> GridSize -> Diagram -> IO (Image PixelRGBA8)
imageOfDiagramAtSize cache gridSize =
  fmap fst . renderSvgDocument cache Nothing 96 . svgOfDiagramAtSize gridSize

-- | Render a Diagram into a PDF file. IO dependency to allow
-- loading of the font files used in the document.
pdfOfDiagram :: FontCache -> Diagram -> IO LB.ByteString
pdfOfDiagram cache =
  fmap fst . pdfOfSvgDocument cache Nothing 96 . svgOfDiagram

-- | Render a Diagram into a PDF file with a custom grid size.
-- IO dependency to allow loading of the font files used in the document.
pdfOfDiagramAtSize :: FontCache -> GridSize -> Diagram -> IO LB.ByteString
pdfOfDiagramAtSize cache size =
  fmap fst . pdfOfSvgDocument cache Nothing 96 . svgOfDiagramAtSize size

-- $linesdoc
-- The basic syntax of asciidiagrams is made of lines made out\nof \'-\' and \'|\' characters. They can be connected with anchors\nlike \'+\' (direct connection) or \'\\\' and \'\/\' (smooth connections)\n
-- 
-- >-----       
-- >  -------   
-- >            
-- >|  |        
-- >|  |        
-- >|  \----    
-- >|           
-- >+-----      
--
-- <<docimages/simple_lines.svg>>
-- 
-- You can use dashed lines by using ':' for vertical lines or '=' for\nhorizontal lines.
-- 
-- 
-- @
--  -----       
--    -=-----   
--              
--  |  :        
--  |  |        
--  |  \\----    
--  |           
--  +--=--      
-- @
-- <<docimages/dashed_lines.svg>>
-- 
-- Arrows are made out of the \'\<\', \'\>\', \'^\' and \'v\'\ncharacters.\nIf the arrows are not connected to any lines, the text is left as is.\n
-- 
-- 
-- @
--      ^
--      |
--      |
-- \<----+----\>
--      |  \< \> v ^
--      |
--      v
-- @
-- <<docimages/arrows.svg>>
-- 

-- $shapesdoc
-- If the lines are closed, then it is detected as such and rendered
-- differently
-- 
-- 
-- @
--   +------+
--   |      |
--   |      +--+
--   |      |  |
--   +---+--+  |
--       |     |
--       +-----+
-- @
-- <<docimages/complexClosed.svg>>
-- 
-- If any of the segment posess one of the dashing markers (\':\' or \'=\')
-- Then the full shape will be dashed.
-- 
-- 
-- @
--   +--+  +--+  +=-+  +=-+
--   |  |  :  |  |  |  |  :
--   +--+  +--+  +--+  +-=+
-- @
-- <<docimages/dashingClosed.svg>>
-- 
-- Any of the angle of a shape can curved one of the smooth corner anchor
-- (\'\\\' or \'\/\')
-- 
-- 
-- @
--   \/--+  +--\\  +--+  \/--+
--   |  |  |  |  |  |  |  |
--   +--+  +--+  \\--+  +--+
-- 
--   \/--+  \/--\\  \/--+  \/--\\ .
--   |  |  |  |  |  |  |  |
--   +--\/  +--+  \\--\/  +--\/
-- 
--   \/--\\ .
--   |  |
--   \\--\/
-- .
-- @
-- <<docimages/curvedCorner.svg>>
-- 

-- $bulletdoc
-- Adding a \'*\' on a line or on a shape add a little circle on it.
-- If the bullet is not attached to any shape or lines, then it
-- will be render like any other text.
-- 
-- 
-- @
--   *-*-*
--   |   |  *----*
--   +---\/       |
--           * * *
-- @
-- <<docimages/bulletTest.svg>>
-- 
-- When used at connection points, it behaves like the \'+\' anchor.
-- 

-- $styledoc
-- The shapes can ba annotated with a tag like `{tagname}`.
-- Tags will be inserted in the class attribute of the shape
-- and can then be stylized with a CSS.
-- 
-- 
-- @
--  +--------+         +--------+
--  | Source +--------\>| op1    |
--  | {src}  |         \\---+----\/
--  +--------+             |
--             +-------*\<--\/
--  +------+\<--| op2   |
--  | Dest |   +-------+
--  |{dst} |
--  +------+
-- 
-- ::: .src { fill: #AAF; }
-- ::: .dst { stroke: #FAA; stroke-width: 3px; }
-- @
-- <<docimages/styleExample.svg>>
-- 
-- Inline css styles are introduced with the ":::" prefix
-- at the beginning of the line. They are introduced in the
-- style section of the generated CSS file
-- 
-- The generated geometry also possess some predefined class
-- which are overidable:
-- 
--  * `dashed_elem` is applyied on every dashed element.
-- 
--  * `filled_shape` is applyied on every closed shape.
-- 
--  * `bullet` on every bullet placed on a shape or line.
-- 
--  * `line_element` on every line element, this include the arrow head.
-- 
-- You can then customize the appearance of the diagram as you want.
-- 

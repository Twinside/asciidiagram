{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This module gives access to the ascii diagram parser and
-- SVG renderer.
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
  ( -- * Main functions
    svgOfDiagram
  , parseAsciiDiagram
  , saveAsciiDiagramAsSvg
  , imageOfDiagram

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

import Data.Monoid( (<>))
import Control.Applicative( (<$>) )
import Control.Monad( forM_ )
import Control.Monad.ST( runST )
import Control.Monad.State.Strict( runState, put, get )
import Data.Function( on )
import Data.List( partition, sortBy )
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
import Graphics.Svg( Dpi, saveXmlFile )
import Graphics.Rasterific.Svg( renderSvgDocument )

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

data HorizontalPoints
  = WithHorizontalSegments
  | WithoutHorizontalSegments
  deriving Eq

allowsHorizontal :: HorizontalPoints -> Bool
allowsHorizontal WithHorizontalSegments = True
allowsHorizontal WithoutHorizontalSegments = False

pointsOfShape :: F.Foldable f => HorizontalPoints -> f Shape -> [Point]
pointsOfShape horizInfo = F.concatMap (F.concatMap go . shapeElements) where
  withHorizontal = allowsHorizontal horizInfo

  go (ShapeAnchor p _) = [p]
  go (ShapeSegment Segment { _segStart = V2 sx sy, _segEnd = V2 ex ey })
    | sx == ex && sy >= ey = [V2 sx yy | yy <- [ey .. sy]]
    | sx == ex             = [V2 sx yy | yy <- [sy .. ey]]
    | withHorizontal && sy == ey && sx >= ex =
        [V2 xx sy | xx <- [ex .. sx]]
    | withHorizontal && sy == ey =
        [V2 xx sy | xx <- [sx .. ex]]
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
  where toSet = [(x + y * _boardWidth board, ' ')
                    | V2 x y <- pointsOfShape WithHorizontalSegments shapes]


pointComp :: Point -> Point -> Ordering
pointComp (V2 x1 y1) (V2 x2 y2) = case compare y1 y2 of
  EQ -> compare x1 x2
  a -> a

rangesOfShapes :: Shape -> [(Point, Point)]
rangesOfShapes shape = pairAssoc sortedPoints
  where
   pairAssoc  [] = []
   pairAssoc [_] = []
   pairAssoc (p1@(V2 _ y1):lst@(p2@(V2 _ y2):rest))
      | y1 == y2 = (p1, p2) : pairAssoc rest
      | otherwise = pairAssoc lst

   sortedPoints = sortBy pointComp
                $ pointsOfShape WithoutHorizontalSegments [shape]


associateTags :: [Shape] -> [TextZone] -> ([Shape], [TextZone])
associateTags shapes tagZones =
  expandTag $ runState (mapM go shapes) sortedZones where

  sortedZones =
    sortBy (pointComp `on` _textZoneOrigin) tagZones

  isInRange (V2 px py) (V2 x1 y1, V2 x2 y2) =
    py == y1 && py == y2 && x1 < px && px < x2
  
  expandTag (s, zones) = (s, fmap expander zones) where
    expander t = t { _textZoneContent = "{" <> _textZoneContent t <> "}" }

  insertAlls shape =
    foldr S.insert (shapeTags shape) . fmap _textZoneContent

  go shape | not $ shapeIsClosed shape = return shape
  go shape = do
    zones <- get

    let ranges = rangesOfShapes shape
        isInShape TextZone { _textZoneOrigin = orig } =
          any (isInRange orig) ranges
        (inRanges, other) = partition isInShape zones

    put other

    return shape { shapeTags = insertAlls shape inRanges }


-- | Analyze an ascii diagram and extract all it's features.
parseAsciiDiagram :: T.Text -> Diagram
parseAsciiDiagram content = Diagram
    { _diagramShapes = S.fromList taggedShape
    , _diagramTexts = zones ++ unusedTags
    , _diagramCellWidth = maximum $ fmap T.length textLines
    , _diagramCellHeight = length textLines
    , _diagramStyles = styleLines
    }
  where
    textLines = T.lines content
    (taggedShape, unusedTags) = associateTags (S.toList validShapes) tags

    (tags, zones) = detectTagFromTextZone
                  $ extractTextZones shapeCleanedText 

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

-- | Render a Diagram as an image. a good value for the Dpi
-- is 96. The IO dependency is there to allow loading of the
-- font files used in the document.
imageOfDiagram :: FontCache -> Dpi -> Diagram -> IO (Image PixelRGBA8)
imageOfDiagram cache dpi = 
  fmap fst . renderSvgDocument cache Nothing dpi . svgOfDiagram


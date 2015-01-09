{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.AsciiDiagram
  ( Diagram( .. ) 
  , TextZone( .. )
  , svgOfDiagram
  , parseAsciiDiagram
  ) where

import Control.Applicative( (<$>) )
import Control.Monad( forM_ )
import Control.Monad.ST( runST )
import Data.Monoid( mempty )
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

{-import Debug.Trace-}
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
    | sy == ey             = [V2 xx sy | xx <- [sx .. ex]]
    | otherwise            = []

cleanupShapes :: (F.Foldable f) => f Shape -> CharBoard -> CharBoard
cleanupShapes shapes board = runST $ do
  mutableBoard <- VU.thaw $ _boardData board
  F.for_ (pointsOfShape shapes) $ \(V2 x y) ->
    let idx = x + y * _boardWidth board in
    VUM.unsafeWrite mutableBoard idx ' '
  outBoard <- VU.unsafeFreeze mutableBoard
  return $ board { _boardData = outBoard }

parseAsciiDiagram :: T.Text -> Diagram
parseAsciiDiagram content = Diagram
    { _diagramShapes = validShapes
    , _diagramTexts = extractTextZones shapeCleanedText
    , _diagramsStyles = mempty
    , _diagramCellWidth = maximum $ fmap T.length textLines
    , _diagramCellHeight = length textLines
    , _diagramStyles = snd <$> styleLine parsed
    }
  where
    textLines = T.lines content
    shapeCleanedText =
      textOfCharBoard . cleanupShapes validShapes $ charBoardOfText textLines
    
    parsed = parseTextLines textLines
    reconstructed =
      reconstruct (anchorMap parsed) $ segmentSet parsed
    validShapes = S.filter isShapePossible reconstructed


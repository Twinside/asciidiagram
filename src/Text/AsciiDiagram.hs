{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.AsciiDiagram
  ( Diagram( .. ) 
  , TextZone( .. )
  , svgOfDiagram
  , parseAsciiDiagram
  ) where

import Data.Monoid( mempty )

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

import Debug.Trace
import Text.Groom
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

cleanupShapes :: (F.Foldable f) => f Shape -> CharBoard -> CharBoard
cleanupShapes shapes board = runST $ do
  mutableBoard <- VU.thaw $ _boardData board
  F.for_ (pointsOfShape WithHorizontalSegments shapes) $ \(V2 x y) ->
    let idx = x + y * _boardWidth board in
    VUM.unsafeWrite mutableBoard idx ' '
  outBoard <- VU.unsafeFreeze mutableBoard
  return $ board { _boardData = outBoard }

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
associateTags shapes tagZones = trace (trace "### TAGZONES:\n" groom sortedZones) $
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


parseAsciiDiagram :: T.Text -> Diagram
parseAsciiDiagram content = Diagram
    { _diagramShapes = S.fromList taggedShape
    , _diagramTexts = zones ++ unusedTags
    , _diagramsStyles = mempty
    , _diagramCellWidth = maximum $ fmap T.length textLines
    , _diagramCellHeight = length textLines
    , _diagramStyles = snd <$> styleLine parsed
    }
  where
    textLines = T.lines content
    (taggedShape, unusedTags) = associateTags (S.toList validShapes) tags

    (tags, zones) = detectTagFromTextZone
                  $ extractTextZones shapeCleanedText 

    shapeCleanedText =
      textOfCharBoard . cleanupShapes validShapes
                      $ charBoardOfText textLines
    
    parsed = parseTextLines textLines
    reconstructed =
      reconstruct (anchorMap parsed) $ segmentSet parsed
    validShapes = S.filter isShapePossible reconstructed


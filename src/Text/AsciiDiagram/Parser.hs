-- | Module in charge of finding the various segment
-- in an ASCII text and the various anchors.
module Text.AsciiDiagram.Parser( ParsingState( .. )
                               , parseText
                               , parseBuffer
                               ) where

import Control.Applicative( (<$>) )
import Control.Monad( foldM )
import Control.Monad.State.Strict( State
                                 , execState
                                 , modify )
import Data.Monoid( (<>), mempty )
import qualified Data.Traversable as TT
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import Linear( V2( .. ) )

import Text.AsciiDiagram.Geometry

isAnchor :: Char -> Bool
isAnchor c = c `VU.elem` anchors
  where
    anchors = VU.fromList "+/\\"
  
anchorOfChar :: Char -> Anchor
anchorOfChar '+' = AnchorMulti
anchorOfChar '/' = AnchorFirstDiag
anchorOfChar '\\' = AnchorSecondDiag
anchorOfChar _ = AnchorMulti

isHorizontalLine :: Char -> Bool
isHorizontalLine c = c `VU.elem` horizontalLineElements
  where
    horizontalLineElements = VU.fromList "-="

isVerticalLine :: Char -> Bool
isVerticalLine c = c `VU.elem` verticalLineElements
  where
    verticalLineElements = VU.fromList ":|"

isBullet :: Char -> Bool
isBullet '*' = True
isBullet _ = False


data ParsingState = ParsingState
    { anchorMap      :: !(M.Map Point Anchor)
    , bulletSet      :: !(S.Set Point)
    , segmentSet     :: !(S.Set Segment)
    , currentSegment :: !(Maybe Segment)
    }
    deriving Show

emptyParsingState :: ParsingState
emptyParsingState = ParsingState
    { anchorMap      = mempty
    , bulletSet      = mempty
    , segmentSet     = mempty
    , currentSegment = Nothing
    }

type Parsing = State ParsingState

type LineNumber = Int

addAnchor :: Point -> Char -> Parsing ()
addAnchor p c = modify $ \s ->
   s { anchorMap = M.insert p (anchorOfChar c) $ anchorMap s }

addSegment :: Segment -> Parsing ()
addSegment seg = modify $ \s ->
   s { segmentSet = S.insert seg $ segmentSet s }

addBullet :: Point -> Parsing ()
addBullet p = modify $ \s ->
   s { bulletSet = S.insert p $ bulletSet s }

continueHorizontalSegment :: Point -> Parsing ()
continueHorizontalSegment p = modify $ \s ->
   s { currentSegment = currentSegment s <> Just (Segment p p SegmentHorizontal) }

stopHorizontalSegment :: Parsing ()
stopHorizontalSegment = modify $ \s ->
    s { segmentSet = inserter (currentSegment s) $ segmentSet s
      , currentSegment = Nothing
      }
  where
    inserter Nothing s = s
    inserter (Just seg) s = S.insert seg s

continueVerticalSegment :: Maybe Segment -> Point -> Parsing (Maybe Segment)
continueVerticalSegment Nothing p = return . Just $ Segment p p SegmentVertical
continueVerticalSegment (Just (Segment start _ _)) p =
    return . Just $ Segment start p SegmentVertical

stopVerticalSegment :: Maybe Segment -> Parsing (Maybe a)
stopVerticalSegment Nothing = return Nothing
stopVerticalSegment (Just seg) = do
    addSegment seg
    return Nothing

parseLine :: [Maybe Segment] -> (LineNumber, T.Text)
          -> Parsing [Maybe Segment]
parseLine prevSegments (lineNumber, txt) = TT.mapM go $ zip3 [0 ..] prevSegments stringLine
  where
    stringLine = T.unpack txt ++ repeat ' '

    go (columnNumber, vertical, c) | isBullet c = do
        let point = V2 columnNumber lineNumber
        addBullet point
        addAnchor point '+'
        stopHorizontalSegment
        stopVerticalSegment vertical

    go (columnNumber, vertical, c) | isHorizontalLine c = do
        let point = V2 columnNumber lineNumber
        continueHorizontalSegment point
        stopVerticalSegment vertical

    go (columnNumber, vertical, c) | isVerticalLine c = do
        let point = V2 columnNumber lineNumber
        stopHorizontalSegment
        continueVerticalSegment vertical point

    go (columnNumber, vertical, c) | isAnchor c = do
        let point = V2 columnNumber lineNumber
        addAnchor point c
        stopHorizontalSegment
        stopVerticalSegment vertical

    go (_, vertical, _) = do
        stopHorizontalSegment
        stopVerticalSegment vertical

maximumLineLength :: [T.Text] -> Int
maximumLineLength [] = 0
maximumLineLength lst = maximum $ T.length <$> lst

parseBuffer :: [T.Text] -> ParsingState
parseBuffer lst = flip execState emptyParsingState $ do
  let initialLine = replicate (maximumLineLength lst) Nothing
  lastVerticalLine <- foldM parseLine initialLine $ zip [0 ..] lst
  mapM_ stopVerticalSegment lastVerticalLine 
    

-- | Extract the segment information of a given text.
parseText :: T.Text -> ParsingState
parseText = parseBuffer . T.lines


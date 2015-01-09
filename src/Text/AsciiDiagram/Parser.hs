{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
-- | Module in charge of finding the various segment
-- in an ASCII text and the various anchors.
module Text.AsciiDiagram.Parser( ParsingState( .. )
                               , parseText
                               , parseTextLines
                               , extractTextZones
                               , detectTagFromTextZone
                               ) where

import Data.Monoid( mempty )

import Control.Applicative( (<$>) )
import Control.Monad( foldM, when )
import Control.Monad.State.Strict( State
                                 , execState
                                 , modify )
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Traversable as TT
import qualified Data.Vector.Unboxed as VU
import Linear( V2( .. ) )

import Text.AsciiDiagram.Geometry

isAnchor :: Char -> Bool
isAnchor c = c `VU.elem` anchors
  where
    anchors = VU.fromList "<>^vV+/\\*"
  
anchorOfChar :: Char -> Anchor
anchorOfChar '+' = AnchorMulti
anchorOfChar '/' = AnchorFirstDiag
anchorOfChar '\\' = AnchorSecondDiag
anchorOfChar '>' = AnchorArrowRight
anchorOfChar '<' = AnchorArrowLeft
anchorOfChar '^' = AnchorArrowUp
anchorOfChar 'V' = AnchorArrowDown
anchorOfChar 'v' = AnchorArrowDown
anchorOfChar '*' = AnchorBullet
anchorOfChar _ = AnchorMulti

isHorizontalLine :: Char -> Bool
isHorizontalLine c = c `VU.elem` horizontalLineElements
  where
    horizontalLineElements = VU.fromList "-="

isVerticalLine :: Char -> Bool
isVerticalLine c = c `VU.elem` verticalLineElements
  where
    verticalLineElements = VU.fromList ":|"

isDashed :: Char -> Bool
isDashed c = case c of
  ':' -> True
  '=' -> True
  _ -> False


data ParsingState = ParsingState
  { anchorMap      :: !(M.Map Point Anchor)
  , segmentSet     :: !(S.Set Segment)
  , currentSegment :: !(Maybe Segment)
  , styleLine      :: [(Int, T.Text)]
  }
  deriving Show

emptyParsingState :: ParsingState
emptyParsingState = ParsingState
    { anchorMap      = mempty
    , segmentSet     = mempty
    , currentSegment = Nothing
    , styleLine      = mempty
    }

type Parsing = State ParsingState

type LineNumber = Int

addAnchor :: Point -> Char -> Parsing ()
addAnchor p c = modify $ \s ->
   s { anchorMap = M.insert p (anchorOfChar c) $ anchorMap s }

addSegment :: Segment -> Parsing ()
addSegment seg = modify $ \s ->
   s { segmentSet = S.insert seg $ segmentSet s }

addStyleLine :: (Int, T.Text) -> Parsing ()
addStyleLine l = modify $ \s ->
   s { styleLine = l : styleLine s }

continueHorizontalSegment :: Point -> Parsing ()
continueHorizontalSegment p = modify $ \s ->
   s { currentSegment = Just . update $ currentSegment s }
  where update Nothing = mempty { _segStart = p, _segEnd = p }
        update (Just seg) = seg { _segEnd = p }

setHorizontaDashing :: Parsing ()
setHorizontaDashing = modify $ \s ->
    s { currentSegment = setDashed <$> currentSegment s }
  where
    setDashed seg = seg { _segDraw = SegmentDashed }

stopHorizontalSegment :: Parsing ()
stopHorizontalSegment = modify $ \s ->
    s { segmentSet = inserter (currentSegment s) $ segmentSet s
      , currentSegment = Nothing
      }
  where
    inserter Nothing s = s
    inserter (Just seg) s = S.insert seg s

continueVerticalSegment :: Maybe Segment -> Point -> Parsing (Maybe Segment)
continueVerticalSegment Nothing p = return $ Just seg where
  seg = mempty { _segStart = p
               , _segEnd = p
               , _segKind = SegmentVertical }
continueVerticalSegment (Just seg) p =
    return $ Just seg { _segEnd = p, _segKind = SegmentVertical }

stopVerticalSegment :: Maybe Segment -> Parsing (Maybe a)
stopVerticalSegment Nothing = return Nothing
stopVerticalSegment (Just seg) = do
    addSegment seg
    return Nothing

parseLine :: [Maybe Segment] -> (LineNumber, T.Text)
          -> Parsing [Maybe Segment]
parseLine prevSegments (n, T.stripPrefix ":::" -> Just txt) = do
    addStyleLine (n, txt)
    return prevSegments
parseLine prevSegments (lineNumber, txt) = TT.mapM go $ zip3 [0 ..] prevSegments stringLine
  where
    stringLine = T.unpack txt ++ repeat ' '

    go (columnNumber, vertical, c) | isHorizontalLine c = do
        let point = V2 columnNumber lineNumber
        continueHorizontalSegment point
        when (isDashed c) $ setHorizontaDashing
        stopVerticalSegment vertical

    go (columnNumber, vertical, c) | isVerticalLine c = do
        let point = V2 columnNumber lineNumber
            dashingSet seg
                | isDashed c = seg { _segDraw = SegmentDashed }
                | otherwise = seg
        stopHorizontalSegment
        fmap dashingSet <$> continueVerticalSegment vertical point

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

parseTextLines :: [T.Text] -> ParsingState
parseTextLines lst = flip execState emptyParsingState $ do
  let initialLine = replicate (maximumLineLength lst) Nothing
  lastVerticalLine <- foldM parseLine initialLine $ zip [0 ..] lst
  mapM_ stopVerticalSegment lastVerticalLine 
    

-- | Extract the segment information of a given text.
parseText :: T.Text -> ParsingState
parseText = parseTextLines . T.lines

zoneFromLine :: (Int, T.Text) -> [TextZone]
zoneFromLine (lineIndex, line) = eatSpaces 0 $ T.split (== ' ') line where
  eatSpaces ix lst = case lst of
     [] -> []
     ("":rest) -> eatSpaces (ix + 1) rest
     _ -> createZoneFrom ix lst

  createZoneFrom ix = go ix where
    go endIdx [] | ix == endIdx = []
    go      _ [] = [TextZone (V2 ix lineIndex) $ T.drop ix line]
    go endIdx ("":rest) = zone : eatSpaces (endIdx + 1) rest
      where origin = V2 ix lineIndex
            zone = TextZone origin . T.drop ix $ T.take endIdx line
    go endIdx (x:xs) = go (endIdx + T.length x + 1) xs

extractTextZones :: [T.Text] -> [TextZone]
extractTextZones = F.concatMap zoneFromLine . zip [0 ..]

detectTagFromTextZone :: [TextZone] -> ([TextZone], [TextZone])
detectTagFromTextZone zones = (concat foundTags, concat normalZones) where
  (foundTags, normalZones) = unzip $ fmap findTag zones

  findTag zone@(TextZone (V2 x y) txt) =
    case splitTags y x $ T.split (== ' ') txt of
      ([], _) -> ([], [zone])
      tagsAndText -> tagsAndText

  splitTags _  _ [] = ([], [])
  splitTags y ix (thisTxt : rest)
     | tlength > 3 && T.head thisTxt == '{' && T.last thisTxt == '}' = 
        (TextZone (V2 y ix) tagText: afterTags, normalTexts)
     | otherwise = (afterTags, TextZone (V2 y ix) thisTxt : normalTexts)
    where tlength = T.length thisTxt
          tagText = T.init $ T.drop 1 thisTxt
          (afterTags, normalTexts) = splitTags y (ix + tlength + 1) rest


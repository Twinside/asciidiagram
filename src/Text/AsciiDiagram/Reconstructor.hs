{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module will try to reconstruct closed shapes and
-- lines from -- the set of anchors and segments.
--
-- The output of this module may be duplicated, needing
-- deduplication as post processing.
--
-- This is mostly a depth first search in the set of anchors
-- and segments.
module Text.AsciiDiagram.Reconstructor( reconstruct ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( mempty )
import Control.Applicative( (<$>) )
#endif

import Control.Monad( when )
import Control.Monad.State.Strict( execState )
import Control.Monad.State.Class( get )
import Data.Function( on )
import Data.List( sortBy )
import Data.Maybe( catMaybes )
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import Linear( V2( .. ), (^+^), (^-^) )

import Text.AsciiDiagram.Geometry
import Text.AsciiDiagram.Graph
import Control.Lens

{-import Debug.Trace-}
{-import Text.Printf-}
{-import Text.Groom-}

data Direction
  = LeftToRight
  | RightToLeft
  | TopToBottom
  | BottomToTop
  | NoDirection
  deriving (Eq, Show)



directionOfVector :: Vector -> Direction
directionOfVector (V2 0 n)
  | n > 0 = TopToBottom
  | otherwise = BottomToTop
directionOfVector (V2 n 0)
  | n > 0 = LeftToRight
  | otherwise = RightToLeft
directionOfVector _ = NoDirection



--
--         ****|****
--      ***    |    ***
--    **       1       **
--   *         |         *
--   -----0----+---2------
--   *         ^         *
--    **       :       **
--      ***    :    ***
--         ****:****
--
--
--         ****|****
--      ***    |    ***
--    **       0       **
--   *         |         *
--   =========>+---1------
--   *         |         *
--    **       2       **
--      ***    |    ***
--         ****|****
--
--
--         ****|****
--      ***    |    ***
--    **       2       **
--   *         |         *
--   -----1----+<=========
--   *         |         *
--    **       0       **
--      ***    |    ***
--         ****|****
--
--
--         ****:****
--      ***    :    ***
--    **       :       **
--   *         V         *
--   -----2----+---0------
--   *         |         *
--    **       1       **
--      ***    |    ***
--         ****|****
--
vectorsForAnchor :: Anchor -> Direction -> [Vector]
vectorsForAnchor anchor dir = case (anchor, dir) of
  (AnchorArrowUp, _) -> [down]
  (AnchorArrowDown, _) -> [up]
  (AnchorArrowLeft, _) -> [right]
  (AnchorArrowRight, _) -> [left]

  (_, LeftToRight) -> [up, right, down, left]
  (_, TopToBottom) -> [right, down, left, up]
  (_, NoDirection) -> [right, down, left, up]
  (_, RightToLeft) -> [down, left, up, right]
  (_, BottomToTop) -> [left, up, right, down]

  where
    left = V2 (-1) 0
    up = V2 0 (-1)
    right = V2 1 0
    down = V2 0 1

directionVectorOf :: Point -> Point -> Vector
directionVectorOf a b = signum <$> a ^-^ b

nextDirectionAfterAnchor :: Anchor -> Point -> Point -> [Point]
nextDirectionAfterAnchor anchor previousPoint anchorPosition =
    [delta | delta <- deltas
           , let nextPoint = anchorPosition ^+^ delta
           , nextPoint /= previousPoint]
  where
    directionVector = directionVectorOf anchorPosition previousPoint
    direction = directionOfVector directionVector
    deltas = vectorsForAnchor anchor direction

nextPointAfterAnchor :: Anchor -> Point -> Point -> [Point]
nextPointAfterAnchor anchor prev p =
    (^+^ p) <$> nextDirectionAfterAnchor anchor prev p

segmentManathanLength :: Segment -> Int
segmentManathanLength seg = x + y where
  V2 x y = abs <$> _segEnd seg ^-^ _segStart seg


segmentDirectionMap :: S.Set Segment -> M.Map Point SegmentKind
segmentDirectionMap = S.fold go mempty where
  go seg = M.insert (_segEnd seg) k . M.insert (_segStart seg) k
    where
      k = _segKind seg

toGraph :: M.Map Point Anchor -> S.Set Segment
        -> Graph Point ShapeElement Segment
toGraph anchors segs = execState graphCreator baseGraph where
  baseGraph = graphOfVertices $ M.mapWithKey ShapeAnchor anchors

  segDirs = segmentDirectionMap segs

  graphCreator = do
    F.traverse_ linkSegments segs
    F.traverse_ linkAnchors $ M.assocs anchors

  linkOf p1 p2 | p1 < p2 = (p1, p2)
               | otherwise = (p2, p1)

  linkAnchors (p, a) = F.traverse_ createLinks nextPoints where
    nextPoints = nextPointAfterAnchor a (V2 (-1) (-1)) p
    createLinks nextPoint = do
      nextExists <- has (vertices . ix nextPoint) <$> get
      let dirNext = nextPoint ^-^ p
          nextP = M.lookup nextPoint anchors
          nextS = M.lookup nextPoint segDirs
          nextIsOk = case (nextP, nextS) of
            (Just AnchorArrowUp, _) -> V2 0 (-1) == dirNext
            (Just AnchorArrowDown, _) -> V2 0 1 == dirNext
            (Just AnchorArrowLeft, _) -> V2 (-1) 0 == dirNext
            (Just AnchorArrowRight, _) -> V2 1 0 == dirNext
            (Just _, _) -> True
            (Nothing, Nothing) -> True
            (Nothing, Just SegmentHorizontal) ->
                (abs <$> dirNext) == V2 1 0
            (Nothing, Just SegmentVertical) ->
                (abs <$> dirNext) == V2 0 1

      alreadyLinked <- has (edges . ix (linkOf p nextPoint)) <$> get
      when (nextExists && nextIsOk && not alreadyLinked) $
         edges . at (linkOf p nextPoint) ?= mempty

  linkSegments seg | segmentManathanLength seg == 0 = do
      vertices . at (_segStart seg ) ?= ShapeSegment seg
  linkSegments seg@(Segment { _segStart = p1, _segEnd = p2 }) = do
      vertices . at p1 ?= ShapeSegment seg
      vertices . at p2 ?= ShapeSegment seg
      edges . at (linkOf p1 p2) ?= seg

findClockwisePossible :: S.Set Point -> Maybe Point -> Point
                      -> [Point]
findClockwisePossible adjacents Nothing p =
    findClockwisePossible adjacents (Just p) p
findClockwisePossible adjacents (Just prev) p =
    fmap snd $ sortBy (compare `on` fst) indexedAdjacents
  where
    -- don't care about specific direction, restrictions should have
    -- been made during the construction of the graph.
    dirArray = V.fromList $ nextDirectionAfterAnchor AnchorMulti prev p
    zipIndex k = (V.elemIndex dir dirArray, k)
      where dir = directionVectorOf k p
    indexedAdjacents =
        [(idx, nextPoint)
                  | (Just idx, nextPoint) <- zipIndex <$> S.elems adjacents
                  , nextPoint /= prev]

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

instance PlanarVertice (V2 Int) where
  getClockwiseMost adj prev =
      safeHead . findClockwisePossible adj prev
  getCounterClockwiseMost adj prev =
      safeHead . reverse . findClockwisePossible adj prev

dedupEqual :: Eq a => [a] -> [a]
dedupEqual [] = []
dedupEqual (x:rest@(y:_)) | x == y = dedupEqual rest
dedupEqual (x:xs) = x : dedupEqual xs


-- | Break filaments at multi anchor to ensure proper dashing
-- of the segments.
breakFilaments :: Filament ShapeElement -> [Filament ShapeElement]
breakFilaments = go where
  go lst = f : fs
    where (f, fs) = breaker lst

  breaker [] = ([], [])
  breaker [a@(ShapeAnchor _ AnchorMulti)] = ([a], [])
  breaker (a@(ShapeAnchor _ AnchorMulti):xs) = ([a], (a:filamentRest):others)
    where (filamentRest, others) = breaker xs
  breaker (x:xs) = (x:filamentRest, others)
    where (filamentRest, others) = breaker xs


-- | Main call of the reconstruction function
reconstruct :: M.Map Point Anchor -> S.Set Segment
            -> S.Set Shape
reconstruct anchors segments =
   S.fromList $ fmap toShapes cycles 
             ++ concatMap toFilaments filaments
  where
    graph = toGraph anchors segments
    (cycles, filaments) = extractAllPrimitives graph

    toElems = dedupEqual
            . filter (/= ShapeSegment mempty)
            . catMaybes
            . fmap (`M.lookup` _vertices graph)

    toFilaments shapes =
      [Shape piece False mempty | piece <- breakFilaments $ toElems shapes] 

    toShapes shapes = Shape (toElems shapes) True mempty


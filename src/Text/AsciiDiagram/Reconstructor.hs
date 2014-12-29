{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Control.Applicative( (<$>) )
import Control.Monad( when )
import Control.Monad.State.Strict( execState )
import Control.Monad.State.Class( get )
import Data.Function( on )
import Data.List( sortBy )
import Data.Maybe( catMaybes )
import Data.Monoid( mempty )
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
vectorsForAnchor :: Direction -> [Vector]
vectorsForAnchor dir = case dir of
  LeftToRight -> [up, right, down, left]
  TopToBottom -> [right, down, left, up]
  NoDirection -> [right, down, left, up]
  RightToLeft -> [down, left, up, right]
  BottomToTop -> [left, up, right, down]

  where
    left = V2 (-1) 0
    up = V2 0 (-1)
    right = V2 1 0
    down = V2 0 1

directionVectorOf :: Point -> Point -> Vector
directionVectorOf a b = signum <$> a ^-^ b

nextDirectionAfterAnchor :: Point -> Point -> [Point]
nextDirectionAfterAnchor previousPoint anchorPosition =
    [delta | delta <- deltas
           , let nextPoint = anchorPosition ^+^ delta
           , nextPoint /= previousPoint]
  where
    directionVector = directionVectorOf anchorPosition previousPoint
    direction = directionOfVector directionVector
    deltas = vectorsForAnchor direction

nextPointAfterAnchor :: Point -> Point -> [Point]
nextPointAfterAnchor prev p =
    (^+^ p) <$> nextDirectionAfterAnchor prev p

segmentManathanLength :: Segment -> Int
segmentManathanLength seg = x + y where
  V2 x y = abs <$> _segEnd seg ^-^ _segStart seg

toGraph :: M.Map Point Anchor -> S.Set Segment
        -> Graph Point ShapeElement Segment
toGraph anchors segs = execState graphCreator baseGraph where
  baseGraph = graphOfVertices $ M.mapWithKey ShapeAnchor anchors

  graphCreator = do
    F.traverse_ linkSegments segs
    F.traverse_ linkAnchors $ M.assocs anchors

  linkOf p1 p2 | p1 < p2 = (p1, p2)
               | otherwise = (p2, p1)

  linkAnchors (p, _) = F.traverse_ createLinks nextPoints where
    nextPoints = nextPointAfterAnchor (V2 (-1) (-1)) p
    createLinks nextPoint = do
      nextExist <- has (vertices . ix nextPoint) <$> get
      alreadyLinked <- has (edges . ix (linkOf p nextPoint)) <$> get
      when (nextExist && not alreadyLinked) $
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
    dirArray = V.fromList $ nextDirectionAfterAnchor prev p
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

-- | Main call of the reconstruction function
reconstruct :: M.Map Point Anchor -> S.Set Segment
            -> S.Set Shape
reconstruct anchors segments =
   S.fromList $ fmap (toShapes True) cycles ++ fmap (toShapes False) filaments
  where
    graph = toGraph anchors segments
    (cycles, filaments) = extractAllPrimitives graph

    toShapes mustClose shapes = Shape shapeElems mustClose where
      shapeElems =
        dedupEqual . filter (/= ShapeSegment mempty)
                   . catMaybes
                   $ fmap (`M.lookup` _vertices graph) shapes


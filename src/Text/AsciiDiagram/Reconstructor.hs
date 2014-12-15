{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
-- | This module will try to reconstruct closed shapes and
-- lines from -- the set of anchors and segments.
--
-- The output of this module may be duplicated, needing
-- deduplication as post processing.
--
-- This is mostly a depth first search in the set of anchors
-- and segments.
module Text.AsciiDiagram.Reconstructor( reconstruct ) where

import Control.Applicative( Applicative, (<$>) )
import Control.Monad( unless )
import Control.Monad.State.Strict( execState )
import Control.Monad.State.Class( MonadState
                                , gets
                                , modify )
import Data.Maybe( catMaybes, fromMaybe )
import Data.Monoid( mempty )
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Map as M
import Linear( V2( .. ), (^+^), (^-^) )

import Text.AsciiDiagram.Geometry
import Text.AsciiDiagram.Deduplicator

{-import Debug.Trace-}
{-import Text.Printf-}

data Direction
  = LeftToRight
  | RightToLeft
  | TopToBottom
  | BottomToTop
  | NoDirection
  deriving (Eq, Show)

type ShapeLocations = M.Map Point ShapeElement


prepareLocationMap :: M.Map Point Anchor -> S.Set Segment
                   -> ShapeLocations
prepareLocationMap anchors = F.foldl' addSegment mapWithAnchors where
  mapWithAnchors = M.mapWithKey ShapeAnchor anchors
  addSegment acc seg = M.insert (_segEnd seg) element
                     $ M.insert (_segStart seg) element acc
    where element = ShapeSegment seg


directionOfVector :: Vector -> Direction
directionOfVector (V2 0 n)
  | n > 0 = TopToBottom
  | otherwise = BottomToTop
directionOfVector (V2 n 0)
  | n > 0 = LeftToRight
  | otherwise = RightToLeft
directionOfVector _ = NoDirection


segmentDirection :: Segment -> Vector
segmentDirection seg = case (_segEnd seg ^-^ _segStart seg, _segKind seg) of
    (V2 0 0, SegmentHorizontal) -> V2 1 0
    (V2 0 0, SegmentVertical) -> V2 0 1
    (vec, _) -> signum vec


vectorsForAnchor :: Anchor -> Direction -> [Vector]
vectorsForAnchor a dir = case (a, dir) of
  (AnchorMulti, _) -> [up, right, down, left]
  (AnchorSecondDiag, _) ->
      negate <$> vectorsForAnchor AnchorFirstDiag dir

  (AnchorFirstDiag, LeftToRight) -> [up]
  (AnchorFirstDiag, TopToBottom) -> [left]
  (AnchorFirstDiag, RightToLeft) -> [down]
  (AnchorFirstDiag, BottomToTop) -> [right]
  (AnchorFirstDiag, NoDirection) -> []

  where
    left = V2 (-1) 0
    up = V2 0 (-1)
    right = V2 1 0
    down = V2 0 1


nextPointAfterSegment :: Point -> Segment -> [(Point, Point)]
nextPointAfterSegment previousPoint seg@(Segment start end _) =
    filter isPointDifferent [(start, start ^-^ dir), (end, end ^+^ dir)]
  where
    isPointDifferent (_, p) = previousPoint /= p
    dir = segmentDirection seg


nextPointAfterAnchor :: Point -> Point -> Anchor -> [Point]
nextPointAfterAnchor previousPoint anchorPosition anchor =
    [nextPoint | delta <- deltas
               , let nextPoint = anchorPosition ^+^ delta
               , nextPoint /= previousPoint]
  where
    directionVector = signum $ anchorPosition ^-^ previousPoint
    direction = directionOfVector directionVector
    deltas = vectorsForAnchor anchor direction


isDirectionIndependant :: ShapeElement -> Bool
isDirectionIndependant (ShapeSegment _) = True
isDirectionIndependant (ShapeAnchor _ anchor) =
  case anchor of
    AnchorMulti -> True
    AnchorFirstDiag -> False
    AnchorSecondDiag -> False


-- | The coordinates on the grid are sure to be in the
-- range ([0..width], [0..height]), so taking negative
-- values ensure that we are in the grid.
unknownStartPoint :: Point
unknownStartPoint = V2 (-10) (-10)


unconsSet :: (Ord a) => S.Set a -> Maybe (a, S.Set a)
unconsSet s | S.null s = Nothing
unconsSet s = Just (mini, S.delete mini s)
  where
    mini = S.findMin s


data ShapeTrace = ShapeTrace
  { shapeIndexInHistory    :: !(M.Map ShapeElement Int)
  , shapeUsedStartPoint    :: !(M.Map ShapeElement (S.Set Point))
  , shapeTraceSize         :: !Int
  , shapeTrace             :: ![ShapeElement]
  , shapeElementSeen       :: S.Set ShapeElement
  , shapeFound             :: S.Set Shape
  }


emptyShapeTrace :: ShapeTrace
emptyShapeTrace = ShapeTrace
  { shapeIndexInHistory = mempty
  , shapeUsedStartPoint = mempty
  , shapeElementSeen    = mempty
  , shapeTrace          = mempty
  , shapeFound          = mempty
  , shapeTraceSize      = 0
  }


safeTail :: [a] -> [a]
safeTail [] = []
safeTail (_:xs) = xs


-- | This function help manage the history of the
-- stack of shape elements during the recursive descent.
withShapeInTrace :: (MonadState ShapeTrace m, Functor m)
                 => ShapeElement -> (Bool -> m a) -> m a
withShapeInTrace shape action = do
  oldIndex <- M.lookup shape <$> gets shapeIndexInHistory
  modify enter
  result <- action $ oldIndex /= Nothing
  leave oldIndex
  return result
  where
    newIndexHistory Nothing history = M.delete shape history
    newIndexHistory (Just ix) history = M.insert shape ix history

    enter strace = strace
        { shapeIndexInHistory =
            M.insert shape currentIndex $ shapeIndexInHistory strace
        , shapeTraceSize = currentIndex + 1
        , shapeTrace = shape : shapeTrace strace
        , shapeElementSeen = S.insert shape $ shapeElementSeen strace
        }
      where
        currentIndex = shapeTraceSize strace

    leave oldIndex = modify $ \strace ->
      strace
          { shapeTraceSize = shapeTraceSize strace - 1
          , shapeTrace = safeTail $ shapeTrace strace
          , shapeIndexInHistory =
              newIndexHistory oldIndex $ shapeIndexInHistory strace
          }


visitedPathOfShapeElement :: (MonadState ShapeTrace m)
                          => ShapeElement -> m (S.Set Point)
visitedPathOfShapeElement el =
  gets $ fromMaybe mempty . M.lookup el . shapeUsedStartPoint


modifyShapePath :: (MonadState ShapeTrace m, Functor m)
                => (S.Set Point -> (a, S.Set Point)) -> ShapeElement -> m a
modifyShapePath f shape = do
  (ret, newPointSet) <- f <$> visitedPathOfShapeElement shape
  modify $ \strace -> strace {
    shapeUsedStartPoint =
        M.insert shape newPointSet $ shapeUsedStartPoint strace }
  return ret


rememberChosenPath :: (MonadState ShapeTrace m, Functor m)
                   => Point -> ShapeElement -> m Bool
rememberChosenPath = modifyShapePath . inserter where
  inserter pt set = (S.member pt set, S.insert pt set)


forgetChosenPath :: (MonadState ShapeTrace m, Functor m)
                 => Point -> ShapeElement -> m ()
forgetChosenPath = modifyShapePath . deleter where
  deleter pt set = ((), S.delete pt set)


isPathAlreadyVisited :: (MonadState ShapeTrace m, Functor m)
                     => Point -> ShapeElement -> m Bool
isPathAlreadyVisited pt el =
    S.member pt <$> visitedPathOfShapeElement el


-- | Mark the incoming point as used to avoid going back.
-- when visiting a segment or anchor. Unmark the point
-- after executing the sub action.
withIncomingPoint :: (MonadState ShapeTrace m, Functor m)
                  => Point -> ShapeElement ->  m a -> m a
withIncomingPoint pt shape act = do
    _previously <- rememberChosenPath pt shape
    ret <- act
    {-unless previously $-}
    forgetChosenPath pt shape
    return ret


data FollowPath = FollowPath
  { _followOrigin :: !Point
  , _followStart  :: !Point
  , _followShape  :: !ShapeElement
  }
  deriving (Eq, Show)


shapesAfterCurrent :: ShapeLocations -> (Point, ShapeElement)
                   -> [FollowPath]
shapesAfterCurrent locations (previousPoint, shape) =
    catMaybes [FollowPath pos p <$> M.lookup p locations
                        | (pos, p) <- pointsAndRoot ]
  where

    pointsAndRoot = case shape of
      ShapeAnchor p a ->
          (p,) <$> nextPointAfterAnchor previousPoint p a
      ShapeSegment segment ->
          nextPointAfterSegment previousPoint segment


saveLoopingUpToIndex :: (MonadState ShapeTrace m) => Int -> m ()
saveLoopingUpToIndex ix = modify go where
  go strace = strace { shapeFound = S.insert shape $ shapeFound strace }
    where
      shape = normalizeClosedShape $ Shape shapeElems True
      shapeElems = take (shapeTraceSize strace - ix) $ shapeTrace strace


-- | Detect if a current shape is in the history meaning that
-- we found a closed shape, and then remember it.
lookupAndSaveCycle :: (MonadState ShapeTrace m, Applicative m)
                   => ShapeElement -> m ()
lookupAndSaveCycle shapeElem = do
  indexHistory <- gets shapeIndexInHistory
  F.traverse_ saveLoopingUpToIndex $ M.lookup shapeElem indexHistory


followShape :: (MonadState ShapeTrace m, Applicative m)
            => ShapeLocations -> (Point, ShapeElement) -> m ()
followShape locations shape@(incomingPoint, shapeElem) = do
  withShapeInTrace shapeElem $ \alreadyPresent ->
    withIncomingPoint incomingPoint shapeElem $ do
      let connectedShapes = shapesAfterCurrent locations shape
      case connectedShapes of
        [] -> return () -- TODO : report a line up to the previous branch
        _ | alreadyPresent -> return ()
        lst -> mapM_ (digChild locations shapeElem) lst

  lookupAndSaveCycle shapeElem


digChild :: (MonadState ShapeTrace m, Applicative m)
         => ShapeLocations -> ShapeElement -> FollowPath -> m ()
digChild locations parentElem child = do
  let branchPoint = _followStart child
  alreadyVisited <- isPathAlreadyVisited branchPoint parentElem
  unless alreadyVisited $ do
    _ <- rememberChosenPath branchPoint parentElem
    followShape locations (_followOrigin child, _followShape child)


-- | Main call of the reconstruction function
reconstruct :: M.Map Point Anchor -> S.Set Segment -> S.Set Shape
reconstruct anchors segments =
    shapeFound $ execState (go starters) emptyShapeTrace
  where
    locations = prepareLocationMap anchors segments
    starters =
        S.fromList . filter isDirectionIndependant $ M.elems locations

    go (unconsSet -> Just (shape, rest)) = do
        followShape locations (unknownStartPoint, shape)
        seens <- gets shapeElementSeen
        go $ rest `S.difference` seens
    go _ = return ()


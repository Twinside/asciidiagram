{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Text.AsciiDiagram.Reconstructor  where

import Control.Applicative( (<$>) )
import Control.Monad( forM_
                    , liftM
                    {-, when-}
                    , unless )
import Control.Monad.State.Strict( execState )
import Control.Monad.State.Class( MonadState
                                {-, get-}
                                {-, put-}
                                , gets
                                , modify )
import Data.Maybe( catMaybes, fromMaybe )
import Data.Monoid( mempty )
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Map as M
import Linear( V2( .. ), (^+^), (^-^) )

import Text.AsciiDiagram.Geometry
import Debug.Trace
import Text.Printf

data ShapeElement
    = ShapeAnchor Point Anchor
    | ShapeSegment Segment
    deriving (Eq, Ord, Show)

data Direction
  = LeftToRight
  | RightToLeft
  | TopToBottom
  | BottomToTop
  | NoDirection
  deriving (Eq, Show)

data Shape = Shape
    { shapeElements :: [ShapeElement]
    , shapeIsClosed :: Bool
    }
    deriving (Eq, Show)

type ShapeLocations = M.Map Point ShapeElement

prepareLocationMap :: M.Map Point Anchor -> S.Set Segment
                   -> ShapeLocations
prepareLocationMap anchors segments =
    F.foldl' addSegment mapWithAnchors segments
  where
    mapWithAnchors = M.mapWithKey ShapeAnchor anchors
    addSegment acc seg = 
        M.insert (_segEnd seg) element $ M.insert (_segStart seg) element acc
      where
        element = ShapeSegment seg

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

nextPointAfterSegment :: String -> Point -> Segment -> [(Point, Point)]
nextPointAfterSegment indent previousPoint seg@(Segment start end _) =
    (\a -> trace (printf "%s| POINT after segment: %s (prev: %s)" indent (show a) (show previousPoint)) a) $
        filter isPointDifferent [(start, start ^-^ dir), (end, end ^+^ dir)]
  where
    isPointDifferent (_, p) = previousPoint /= p
    dir = segmentDirection seg

nextPointAfterAnchor :: String -> Point -> Point -> Anchor -> [Point]
nextPointAfterAnchor indent previousPoint anchorPosition anchor =
    (\a -> trace (printf "%s| POINT after anchor: %s (prev: %s)" indent (show a) (show previousPoint)) a) $
    [nextPoint | delta <- deltas
               , let nextPoint = anchorPosition ^+^ delta
               , nextPoint /= previousPoint]
  where
    directionVector = signum $ anchorPosition ^-^ previousPoint
    {-previousPoint = anchorPosition ^-^ directionVector-}
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
  , shapeFound             :: [Shape]
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

withShapeInTrace :: (MonadState ShapeTrace m)
                 => String -> ShapeElement -> m a -> m a
withShapeInTrace indent shape action = trace (printf "%s| PUSHING %s" indent $ show shape) $ do
  oldIndex <- M.lookup shape `liftM` gets shapeIndexInHistory
  modify enter
  result <- action
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
        trace (printf "%s| BACKTRACKING>" indent) $
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

modifyShapePath :: (MonadState ShapeTrace m)
                => (S.Set Point -> (a, S.Set Point)) -> ShapeElement -> m a
modifyShapePath f shape = do
  (ret, newPointSet) <- f `liftM` visitedPathOfShapeElement shape
  modify $ \strace -> strace {
    shapeUsedStartPoint =
        M.insert shape newPointSet $ shapeUsedStartPoint strace }
  return ret

rememberChosenPath :: (MonadState ShapeTrace m) => Point -> ShapeElement -> m Bool
rememberChosenPath = modifyShapePath . inserter where
  inserter pt set = (S.member pt set, S.insert pt set)

forgetChosenPath :: (MonadState ShapeTrace m) => Point -> ShapeElement -> m ()
forgetChosenPath = modifyShapePath . deleter where
  deleter pt set = ((), S.delete pt set)

isPathAlreadyVisited :: (MonadState ShapeTrace m)
                     => Point -> ShapeElement -> m Bool
isPathAlreadyVisited pt el =
    S.member pt `liftM` visitedPathOfShapeElement el

withIncomingPoint :: (MonadState ShapeTrace m)
              => Point -> ShapeElement ->  m a -> m a
withIncomingPoint pt shape act = do
    previously <- rememberChosenPath pt shape
    ret <- act
    unless previously $
        forgetChosenPath pt shape
    return ret

shapesAfterCurrent :: (MonadState ShapeTrace m)
                   => String -> ShapeLocations -> (Point, ShapeElement)
                   -> m [(Point, ShapeElement)]
shapesAfterCurrent indent locations (previousPoint, shape) = do
    visited <- visitedPathOfShapeElement shape
    return . (\a -> trace (printf "%s| Shape after current: %s" indent $ show a) a)
           $ shapesOfPoint pointsAndRoot visited
  where
    shapesOfPoint points visited =
      catMaybes [(pos,) <$> M.lookup p locations | (pos, p) <- points 
                                                 , pos `S.notMember` visited
                                                 ]

    pointsAndRoot = case shape of
      ShapeAnchor p a ->
          (p,) <$> nextPointAfterAnchor indent previousPoint p a
      ShapeSegment segment ->
          nextPointAfterSegment indent previousPoint segment


saveLoopingUpToIndex :: (MonadState ShapeTrace m) => Int -> m ()
saveLoopingUpToIndex ix = modify go where 
  go strace = strace { shapeFound = Shape shapeElems True : shapeFound strace }
    where
      shapeElems = take (shapeTraceSize strace - ix) $ shapeTrace strace
  

filterSaveLooping :: (MonadState ShapeTrace m)
                  => [(a, ShapeElement)]
                  -> m [(a, ShapeElement)]
filterSaveLooping rootList = do
  indexHistory <- gets shapeIndexInHistory
  let go [] = return []
      go lst@(x@(_,shape):xs) = case M.lookup shape indexHistory of
        Nothing -> (x :) `liftM` go xs
        Just ix -> do
          saveLoopingUpToIndex ix
          return lst
  go rootList

reconstruct :: M.Map Point Anchor -> S.Set Segment -> [Shape]
reconstruct anchors segments =
    shapeFound $ execState (go starters) emptyShapeTrace
  where
    locations = prepareLocationMap anchors segments
    starters =
        S.fromList . filter isDirectionIndependant $ M.elems locations

    go (unconsSet -> Just (shape, rest)) = do
        follow "" (unknownStartPoint, shape)
        seens <- gets shapeElementSeen
        go $ rest `S.difference` seens
    go _ = return ()
      
    follow indent shape@(incomingPoint, shapeElem) =
      let nextIndent = indent ++ "  " in
      withShapeInTrace indent shapeElem $
        withIncomingPoint incomingPoint shapeElem $ do
          connectedShapes <- shapesAfterCurrent indent locations shape
          forM_ connectedShapes $ \ nextShape@(branchPoint, _) -> do
              alreadyVisited <- isPathAlreadyVisited branchPoint shapeElem
              unless alreadyVisited $ do
                _ <- rememberChosenPath branchPoint shapeElem
                follow nextIndent nextShape


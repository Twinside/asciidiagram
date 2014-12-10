{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Text.AsciiDiagram.Reconstructor  where

import Control.Applicative( (<$>) )
import Control.Monad( liftM )
import Control.Monad.State.Strict( execState )
import Control.Monad.State.Class( MonadState
                                , gets
                                , modify )
import Data.Maybe( catMaybes )
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
segmentDirection seg = 
    signum $ _segStart seg ^-^ _segEnd seg

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

nextPointAfterSegment :: Point -> Segment -> [Point]
nextPointAfterSegment p seg =
    [nextPoint | delta <- [dir, negate <$> dir]
               , let nextPoint = p ^+^ delta
               , nextPoint /= p]
  where
    dir = segmentDirection seg

nextPointAfterAnchor :: Point -> Point -> Anchor -> [Point]
nextPointAfterAnchor pointSource anchorPosition anchor =
    [nextPoint | delta <- deltas
               , let nextPoint = anchorPosition ^+^ delta
               , nextPoint /= previousPoint]
  where
    directionVector = signum $ anchorPosition ^-^ pointSource
    previousPoint = anchorPosition ^-^ directionVector
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
  { shapeIndexInHistory :: !(M.Map ShapeElement Int)
  , shapeTraceSize :: !Int
  , shapeTrace :: ![ShapeElement]
  , shapes :: [Shape]
  , shapeElementSeens :: S.Set ShapeElement
  }

emptyShapeTrace :: ShapeTrace
emptyShapeTrace = ShapeTrace
  { shapeIndexInHistory = mempty
  , shapeTraceSize = 0
  , shapeTrace = mempty
  , shapeElementSeens = mempty
  , shapes = mempty
  }

pushShape :: (MonadState ShapeTrace m) => ShapeElement -> m ()
pushShape shape = modify $ \strace ->
  let currentIndex = shapeTraceSize strace in
  strace
    { shapeIndexInHistory =
        M.insert shape currentIndex $ shapeIndexInHistory strace
    , shapeTraceSize = succ currentIndex
    , shapeTrace = shape : shapeTrace strace
    }

isInHistory :: (MonadState ShapeTrace m) => ShapeElement -> m (Maybe Int)
isInHistory s =
  -- should be <$> instead of liftM, but Functor is not superclass
  -- of Monad as time of writing.
  M.lookup s `liftM` gets shapeIndexInHistory

shapesAfterCurrent :: ShapeLocations -> Point -> ShapeElement
                   -> [(Point, ShapeElement)]
shapesAfterCurrent locations pt shape =
    catMaybes [(p,) <$> M.lookup p locations | p <- points]
  where
    points = case shape of
      ShapeAnchor p a -> nextPointAfterAnchor pt p a
      ShapeSegment segment -> nextPointAfterSegment pt segment


saveLoopingUpToIndex :: (MonadState ShapeTrace m) => Int -> m ()
saveLoopingUpToIndex ix = modify $ \strace ->
  let shapeElems = take (shapeTraceSize strace - ix) $ shapeTrace strace
  in
  strace { shapes = Shape shapeElems True : shapes strace
         , shapeElementSeens =
             F.foldr S.delete (shapeElementSeens strace) shapeElems
         }

backtrack :: (MonadState ShapeTrace m) => m ()
backtrack = modify $ \strace ->
  strace
    { shapeTraceSize = shapeTraceSize strace - 1
    , shapeTrace = tail $ shapeTrace strace
    , shapeIndexInHistory =
        M.delete (head $ shapeTrace strace) $ shapeIndexInHistory strace
    }

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
    shapes $ execState (go starters) emptyShapeTrace
  where
    locations = prepareLocationMap anchors segments
    starters =
        S.fromList . filter isDirectionIndependant $ M.elems locations

    go (unconsSet -> Just (shape, rest)) = trace (printf "TOP level:%s" $ show shape) $ do
        follow (unknownStartPoint, shape)
        allSeens <- gets shapeElementSeens
        go $ rest `S.difference` allSeens
    go _ = return ()
      
    follow (pt, shape) = trace (printf "In shape %s" $ show shape) $do
      pushShape shape
      let connectedShapes = shapesAfterCurrent locations pt shape
      shapeToFollow <- filterSaveLooping connectedShapes 
      mapM_ follow shapeToFollow
      backtrack
-- -}


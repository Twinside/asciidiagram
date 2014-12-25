{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Text.AsciiDiagram.Graph
  ( Graph( .. )
  , graphOfVertices
  , extractAllPrimitives
  , addVertice
  , connect
  , vertices
  , edges
  ) where

import Control.Applicative( (<$>) )
import Control.Monad( forM_, when )
import Control.Monad.State.Strict( execState )
import Control.Monad.State.Class( MonadState
                                , gets)
import Data.Function( on )
import Data.Maybe( fromMaybe )
import Data.Monoid( Monoid( .. ), mempty )
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Lens( Lens'
                   , lens
                   , (&)
                   , (.~)
                   , (?~)
                   , (%=)
                   , (.=)
                   , itraverse_
                   , contains
                   , at
                   , use
                   )

data Graph vertex vinfo edgeInfo = Graph
  { _vertices :: M.Map vertex vinfo
  , _edges    :: M.Map (vertex, vertex) edgeInfo
  }
  deriving (Eq, Ord)

vertices :: Lens' (Graph vertex vinfo edgeInfo) (M.Map vertex vinfo)
vertices = lens _vertices setVertices where
  setVertices g v = g { _vertices = v }

edges :: Lens' (Graph vertex vinfo edgeInfo)
               (M.Map (vertex, vertex) edgeInfo)
edges = lens _edges setEdge where
  setEdge g e = g { _edges = e }

graphOfVertices :: (Ord vertex) => M.Map vertex vinfo -> Graph vertex vinfo a
graphOfVertices vertMap = emptyGraph & vertices .~ vertMap 

emptyGraph :: (Ord v) => Graph v vi e
emptyGraph = Graph
  { _vertices = mempty
  , _edges = mempty
  }

instance (Ord v) => Monoid (Graph v vi e) where
  mempty = emptyGraph
  mappend a b = Graph
    { _vertices = (mappend `on` _vertices) a b
    , _edges = (mappend `on` _edges) a b
    }

addVertice :: Ord v
           => v -> vinfo -> Graph v vinfo edgeInfo
           -> Graph v vinfo edgeInfo
addVertice v info g = g & vertices . at v ?~ info


connect :: Ord v
        => v -> v -> edgeInfo -> Graph v vinfo edgeInfo
        -> Graph v vinfo edgeInfo
connect a b info g = g & edges . at (linkOf a b)  ?~ info

adjacencyMapOfGraph :: (Ord v) => Graph v vi ei -> M.Map v (Int, S.Set v)
adjacencyMapOfGraph = flip execState mempty . itraverse_ go . _edges where
  inserter p Nothing = Just (1, S.singleton p)
  inserter p (Just (n, s)) = Just (n + 1, S.insert p s)

  go (k1, k2) _ = do
    at k1 %= inserter k2
    at k2 %= inserter k1

type Filament v = [v]
type Cycle v = [v]

data MinimalCycleFinderState v vi ei = MinimalCycleFinderState
  { _adjacency      :: M.Map v (Int, S.Set v)
  , _graph          :: Graph v vi ei
  , _visited        :: S.Set v
  , _cycleEdges     :: S.Set (v, v)
  , _foundFilaments :: [Filament v]
  , _foundCycles    :: [Cycle v]
  }

emptyCycleFinderState :: (Ord v)
                      => Graph v vi ei -> MinimalCycleFinderState v vi ei 
emptyCycleFinderState g = MinimalCycleFinderState
  { _adjacency = adjacencyMapOfGraph g
  , _graph = g
  , _visited = mempty
  , _cycleEdges = mempty
  , _foundFilaments = mempty
  , _foundCycles = mempty
  }


visited :: Lens' (MinimalCycleFinderState v vi ei)
                 (S.Set v)
visited = lens _visited setter where
  setter a b = a { _visited = b }

foundFilaments :: Lens' (MinimalCycleFinderState v vi ei)
                        [Filament v]
foundFilaments = lens _foundCycles setter where
  setter a b = a { _foundFilaments = b }

foundCycles :: Lens' (MinimalCycleFinderState v vi ei)
                     [Cycle v]
foundCycles = lens _foundCycles setter where
  setter a b = a { _foundCycles = b }

cycleEdges :: Lens' (MinimalCycleFinderState v vi ei)
                    (S.Set (v, v))
cycleEdges = lens _cycleEdges setter where
  setter a b = a { _cycleEdges = b }

adjacency :: Lens' (MinimalCycleFinderState v vi ei)
                   (M.Map v (Int, S.Set v))
adjacency = lens _adjacency setter where
  setter a b = a { _adjacency = b }

graph :: Lens' (MinimalCycleFinderState v vi ei)
               (Graph v vi ei)
graph = lens _graph  setter where
  setter a b = a { _graph = b }

linkOf :: (Ord v) => v -> v -> (v, v)
linkOf p1 p2 | p1 < p2 = (p1, p2)
             | otherwise = (p2, p1)


isInCycle :: (Ord v, MonadState (MinimalCycleFinderState v vi ei) m)
          => v -> v -> m Bool
isInCycle a b = use $ cycleEdges . contains (linkOf a b)

removeEdge :: ( MonadState (MinimalCycleFinderState v vi ei) m
              , Ord v )
           => v -> v -> m ()
removeEdge a b = do
  let remEdge p (n, s) = (n - 1, S.delete p s)
  adjacency . at a %= fmap (remEdge b)
  adjacency . at b %= fmap (remEdge a)
  graph . edges . at (linkOf a b) .= Nothing


removeVertice :: ( MonadState (MinimalCycleFinderState v vi ei) m
                 , Ord v )
              => v -> m ()
removeVertice v =
  graph . vertices . at v .= Nothing

adjacencyInfoOfVertice :: ( MonadState (MinimalCycleFinderState v vi ei) m
                          , Ord v
                          , Functor m )
                       => v -> m (Int, S.Set v)
adjacencyInfoOfVertice v =
  fromMaybe (0, mempty) <$> use (adjacency . at v)

extractFilament :: ( MonadState (MinimalCycleFinderState v vi ei) m
                   , Ord v
                   , Functor m )
                => v -> v -> m [v]
extractFilament fromVertice toVertice = do
  mustCycle <- isInCycle fromVertice toVertice
  (fromCount, _) <- adjacencyInfoOfVertice fromVertice
  (toCount, toAdjacents) <- adjacencyInfoOfVertice toVertice
  if fromCount >= 3 then do
    removeEdge fromVertice toVertice
    let startVertice
          | toCount == 1 = S.findMin toAdjacents
          | otherwise = toVertice
    follow mustCycle [fromVertice] startVertice
  else
    follow mustCycle [] fromVertice
  where
    follow mustCycle trace currentVertice = do
      (count, adjacent) <- adjacencyInfoOfVertice currentVertice
      case count of
        0 -> do
          removeVertice currentVertice
          return $ currentVertice : trace

        1 -> do
          let nextVertice = S.findMin adjacent
          inCycle <- isInCycle currentVertice nextVertice
          if mustCycle && not inCycle then
            return $ currentVertice : trace
          else do
            removeEdge currentVertice nextVertice
            removeVertice currentVertice
            follow mustCycle (currentVertice : trace) nextVertice

        _ ->
          return $ currentVertice : trace

class Ord v => PlanarVertice v where
  getClockwiseMost :: M.Map v (Int, S.Set v) -> Maybe v -> v
                   -> Maybe v
  getCounterClockwiseMost :: M.Map v (Int, S.Set v) -> Maybe v -> v
                          -> Maybe v

findClockwiseMost :: ( MonadState (MinimalCycleFinderState v vi ei) m
                     , PlanarVertice v )
                  => Maybe v -> v -> m (Maybe v)
findClockwiseMost mv v = do
  adj <- gets _adjacency
  return $ getClockwiseMost adj mv v

findCounterClockwiseMost
    :: ( MonadState (MinimalCycleFinderState v vi ei) m
       , PlanarVertice v )
    => Maybe v -> v -> m (Maybe v)
findCounterClockwiseMost mv v = do
  adj <- gets _adjacency
  return $ getCounterClockwiseMost adj mv v

extractFilamentFromMiddle
  :: ( MonadState (MinimalCycleFinderState v vi ei) m
     , Functor m
     , Ord v )
  => v -> v -> m [v]
extractFilamentFromMiddle = go where
  go prev curr = do
    (adjCount, adjs) <- adjacencyInfoOfVertice curr
    let nextVertice = S.findMin adjs
    if adjCount /= 2 then
      extractFilament curr prev
    else if prev /= nextVertice then
      go curr nextVertice
    else
      go curr $ S.elemAt 1 adjs

extractPrimitive :: ( MonadState (MinimalCycleFinderState v vi ei) m
                    , Functor m 
                    , PlanarVertice v )
                 => v -> m ()
extractPrimitive rootNode = do
  startNode <- findClockwiseMost Nothing rootNode
  let starting = fromMaybe rootNode startNode

      follow _trace prevVertice Nothing = do
        filament <- extractFilament prevVertice prevVertice
        foundFilaments %= (filament:)
      follow trace _ (Just v) | v == rootNode = do
        foundCycles %= (trace :)
        forM_ (zip trace $ tail trace) $ \(a, b) ->
          cycleEdges . contains (linkOf a b)  .= True
        removeEdge rootNode starting
        extractIfAlone rootNode
        extractIfAlone starting
      follow trace prevVertice (Just v) = do
        wasVisited <- use $ visited . contains v
        if wasVisited then do
          filament <- extractFilamentFromMiddle starting rootNode
          foundFilaments %= (filament:)
        else do
          nextVertice <-
              findCounterClockwiseMost (Just prevVertice) v
          follow (v:trace) v nextVertice

  follow [rootNode] rootNode startNode
  where
    extractIfAlone node = do
      (startCount, adjs) <- adjacencyInfoOfVertice node
      when (startCount == 1) $ do
        filament <- extractFilament node $ S.findMin adjs
        foundFilaments %= (filament:)

extractAllPrimitives :: (PlanarVertice v)
                     => Graph v vi ei -> ([Cycle v], [Filament v])
extractAllPrimitives initGraph = extract $ execState go initialState where
  initialState = emptyCycleFinderState initGraph
  extract s = (_foundCycles s, _foundFilaments s)

  go = do
    vs <- use $ graph . vertices
    if M.null vs then return ()
    else do
      extractPrimitive $ fst $ M.findMin vs
      go

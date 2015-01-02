module Text.AsciiDiagram.Geometry( Point
                                 , Vector
                                 , Anchor( .. )
                                 , Segment( .. )
                                 , SegmentKind( .. )
                                 , Shape( .. )
                                 , ShapeElement( .. )
                                 , firstPointOfShape
                                 ) where

import Data.Monoid( Monoid( mappend, mempty ))
import Linear( V2( .. ) )

type Point = V2 Int
type Vector = V2 Int

data Anchor
  = AnchorMulti       -- ^ Associated to '+'
  | AnchorFirstDiag   -- ^ Associated to '/'
  | AnchorSecondDiag  -- ^ Associated to '\'
  | AnchorPoint       -- ^ Kind of "end anchor", without continuation.
  deriving (Eq, Ord, Show)

data SegmentKind
  = SegmentHorizontal
  | SegmentVertical
  deriving (Eq, Ord, Show)

data SegmentDraw
  = SegmentSolid
  | SegmentDashed
  deriving (Eq, Ord, Show)

data Segment = Segment
  { _segStart :: {-# UNPACK #-} !Point
  , _segEnd   :: {-# UNPACK #-} !Point
  , _segKind  :: !SegmentKind
  , _segDraw  :: !SegmentDraw
  }
  deriving (Eq, Ord)

instance Show Segment where
    showsPrec d (Segment s e k dr) =
      showParen (d >= 10) $
          showString "Segment " . start . end . kind . (' ':) . draw
      where
        start = showParen True $ shows s
        end = showParen True $ shows e
        kind = shows k
        draw = shows dr

instance Monoid Segment where
    mempty = Segment 0 0 SegmentVertical SegmentDashed
    mappend (Segment a _ _ _) (Segment _ b k d) =
        Segment a b k d

data ShapeElement
  = ShapeAnchor !Point !Anchor
  | ShapeSegment !Segment
  deriving (Eq, Ord, Show)


data Shape = Shape
  { shapeElements :: [ShapeElement]
  , shapeIsClosed :: Bool
  }
  deriving (Eq, Ord, Show)

firstPointOfShape :: [ShapeElement] -> Point
firstPointOfShape lst = case lst of
  [] -> V2 (-10) (-10)
  ShapeAnchor p _ : _ -> p
  ShapeSegment seg : _ -> _segStart seg


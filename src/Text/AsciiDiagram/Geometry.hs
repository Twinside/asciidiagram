module Text.AsciiDiagram.Geometry( Point
                                 , Vector
                                 , Anchor( .. )
                                 , Segment( .. )
                                 , SegmentKind( .. )
                                 ) where

import Data.Monoid( Monoid( mappend, mempty ))
import Linear( V2( .. ) )

type Point = V2 Int
type Vector = V2 Int

data Anchor
    = AnchorMulti       -- ^ Associated to '+'
    | AnchorFirstDiag   -- ^ Associated to '/'
    | AnchorSecondDiag  -- ^ Associated to '\'
    deriving (Eq, Ord, Show)

data SegmentKind
  = SegmentHorizontal
  | SegmentVertical
  deriving (Eq, Ord, Show)

data Segment = Segment
  { _segStart :: {-# UNPACK #-} !Point
  , _segEnd   :: {-# UNPACK #-} !Point
  , _segKind  :: !SegmentKind
  }
  deriving (Eq, Ord)

instance Show Segment where
    showsPrec _d (Segment s e k) =
      showString "Segment " . start . end . kind
        where
          start = showParen True $ shows s
          end = showParen True $ shows e
          kind = showParen True $ shows k

instance Monoid Segment where
    mempty = Segment 0 0 SegmentVertical
    mappend (Segment a _ _) (Segment _ b k) = Segment a b k


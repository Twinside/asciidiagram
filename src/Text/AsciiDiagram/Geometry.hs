module Text.AsciiDiagram.Geometry( Point
                                 , Anchor( .. )
                                 , Segment( .. )
                                 )where

import Data.Monoid( Monoid( mappend, mempty ))
import Linear( V2( .. ) )

type Point = V2 Int

data Anchor
    = AnchorMulti       -- ^ Associated to '+'
    | AnchorFirstDiag   -- ^ Associated to '/'
    | AnchorSecondDiag  -- ^ Associated to '\'
    deriving (Eq, Ord, Show)

data Segment = Segment
  { _segStart :: {-# UNPACK #-} !Point
  , _segEnd   :: {-# UNPACK #-} !Point
  }
  deriving (Eq, Ord, Show)

instance Monoid Segment where
    mempty = Segment 0 0
    mappend (Segment a _) (Segment _ b) = Segment a b


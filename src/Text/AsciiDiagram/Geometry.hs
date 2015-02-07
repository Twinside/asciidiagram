{-# LANGUAGE CPP #-}
-- | Defines the geometry extracted from the asciidiagram.
module Text.AsciiDiagram.Geometry( Point
                                 , Vector
                                 , Anchor( .. )
                                 , Shape( .. )
                                 , Segment( .. )
                                 , SegmentDraw( .. )
                                 , SegmentKind( .. )
                                 , ShapeElement( .. )
                                 , Diagram( .. )
                                 , TextZone( .. )
                                 ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( Monoid( mappend, mempty ))
#endif

import qualified Data.Set as S
import qualified Data.Text as T
import Linear( V2( .. ) )

-- | Position of a an element in the character grids
type Point = V2 Int

-- | Direction in the character grid.
type Vector = V2 Int

-- | Describe the geomtry of a full Ascii Diagram
-- document
data Diagram = Diagram
  { -- | All the extracted tshapes
    _diagramShapes     :: S.Set Shape
    -- | All the extracted text zones
  , _diagramTexts      :: [TextZone]
    -- | Width in characters of the document
  , _diagramCellWidth  :: !Int
    -- | Height in characters of the document
  , _diagramCellHeight :: !Int
    -- | CSS styles associated to the document.
  , _diagramStyles     :: [T.Text]
  }
  deriving (Eq, Show)

-- | Describe where to place a text snippet.
data TextZone = TextZone
  { _textZoneOrigin  :: Point
  , _textZoneContent :: T.Text
  }
  deriving (Eq, Show)

-- | Define the different connection points
-- between the segments.
data Anchor
  = AnchorMulti       -- ^ Associated to '+'
  | AnchorFirstDiag   -- ^ Associated to '/'
  | AnchorSecondDiag  -- ^ Associated to '\'

  | AnchorPoint       -- ^ Kind of "end anchor", without continuation.
  | AnchorBullet      -- ^ Used as a '*'

  | AnchorArrowUp     -- ^ Associated to '^'
  | AnchorArrowDown   -- ^ Associated to 'V'
  | AnchorArrowLeft   -- ^ Associated to '<'
  | AnchorArrowRight  -- ^ Associated to '>'
  deriving (Eq, Ord, Show)

-- | Helper data for segment composed of only
-- one character
data SegmentKind
  = SegmentHorizontal
  | SegmentVertical
  deriving (Eq, Ord, Show)

-- | Differentiate between elements drawn with a
-- solid line or with dashed lines.
data SegmentDraw
  = SegmentSolid
  | SegmentDashed
  deriving (Eq, Ord, Show)

-- | Define an horizontal or vertical segment.
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
    mempty = Segment 0 0 SegmentVertical SegmentSolid
    mappend (Segment a _ _ _) (Segment _ b k d) =
        Segment a b k d

-- | Describe a composant of shapes. Can
-- be composed of anchors like "+*/\\" or
-- segments.
data ShapeElement
  = ShapeAnchor !Point !Anchor
  | ShapeSegment !Segment
  deriving (Eq, Ord, Show)

-- | Shape extracted from the text
data Shape = Shape
  { -- | Elements composing the shape.
    shapeElements :: [ShapeElement]
    -- | When False it's just lines possibly with
    -- an arrow, when True it's a polygon.
  , shapeIsClosed :: Bool
    -- | Tags "{tagname}" placed inside the shape.
  , shapeTags     :: S.Set T.Text
  }
  deriving (Eq, Ord, Show)


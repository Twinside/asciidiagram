module Text.AsciiDiagram.DiagramCleaner
    ( isShapePossible
    ) where

import Control.Applicative( (<$>), (<*>) )
import Data.List( tails )
import Text.AsciiDiagram.Geometry
import Linear( V2( V2 )
             , (^-^)
             )

isShapePossible :: Shape -> Bool
isShapePossible shape = all check elements
  where
    elements
        | shapeIsClosed shape =
            (++ shapeElements shape) <$> tails (shapeElements shape) 
        | otherwise = tails (shapeElements shape)


    --   dir                fromS1
    --  --->               ----->
    --   /\           | 1 /---- 0
    -- 1/  \2     dir |  /
    --  |  |          |  \
    -- 0|  |3         v 2 \---- 3
    --                     ----->
    --                      fromS2
    --     fromS1
    --    <----
    --   0 ----/ 1 |
    --        /    | dir
    --        \    |
    --   3 ----\ 2 v
    --    <----
    --    fromS2
    --
    --     ^ 2 \--- 3
    --     |    \
    -- dir |    /
    --     | 1 /--- 0
    --
    check ( ShapeSegment s1
          : ShapeAnchor ap1 AnchorFirstDiag   -- '/'
          : ShapeAnchor ap2 AnchorSecondDiag  -- '\'
          : ShapeSegment s2
          : _) = okX && okY
      where V2 dirX dirY = ap2 ^-^ ap1
            fromS1 = _segEnd s1 ^-^ ap1
            fromS2 = _segStart s2 ^-^ ap2 

            signDirs = signum <$> V2 dirY dirX

            (&&&) a b = (&&) <$> a <*> b
            (===) a b = (==) <$> a <*> b
            V2 okX okY =
                (signDirs === (signum <$> fromS1)) &&&
                (signDirs === (signum <$> fromS2))

    --   dir                fromS2
    --  <---               ----->
    --   /\           ^ 2 /---- 3
    -- 2/  \1     dir |  /
    --  |  |          |  \
    -- 3|  |0         | 1 \---- 0
    --                     ----->
    --                      fromS1
    --     fromS1
    --    <----
    --   3 ----/ 2 ^
    --        /    | dir
    --        \    |
    --   0 ----\ 1 |
    --    <----
    --    fromS2
    --
    --     | 1 \--- 0
    --     |    \
    -- dir |    /
    --     v 2 /--- 3
    --
    check ( ShapeSegment s1
          : ShapeAnchor ap1 AnchorSecondDiag  -- '\'
          : ShapeAnchor ap2 AnchorFirstDiag   -- '/'
          : ShapeSegment s2
          : _) = okX && okY
      where V2 dirX dirY = ap2 ^-^ ap1
            fromS1 = _segEnd s1 ^-^ ap1
            fromS2 = _segStart s2 ^-^ ap2 

            signDirs = signum <$> V2 dirY dirX

            (&&&) a b = (&&) <$> a <*> b
            (/==) a b = diffSign <$> a <*> b where
              diffSign 0 0 = True
              diffSign aa bb = aa /= bb
              

            V2 okX okY =
                (signDirs /== (signum <$> fromS1)) &&&
                (signDirs /== (signum <$> fromS2))

    check ( ShapeAnchor _ AnchorFirstDiag
          : ShapeAnchor _ AnchorSecondDiag
          : ShapeAnchor _ AnchorFirstDiag
          : ShapeAnchor _ AnchorSecondDiag
          : _) = False

    check ( ShapeAnchor _ AnchorSecondDiag
          : ShapeAnchor _ AnchorFirstDiag
          : ShapeAnchor _ AnchorSecondDiag
          : ShapeAnchor _ AnchorFirstDiag
          : _) = False

    check [ ShapeAnchor _ AnchorFirstDiag
          , ShapeAnchor _ AnchorSecondDiag] = False

    check [ ShapeAnchor _ AnchorSecondDiag
          , ShapeAnchor _ AnchorFirstDiag] = False

    check _ = True



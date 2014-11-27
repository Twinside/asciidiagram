module Text.AsciiDiagram.Parser where

import qualified Data.Vector.Unboxed as VU

isAnchor :: Char -> Bool
isAnchor c = c `VU.elem` anchors
  where
    anchors = VU.fromList ['+', '/', '\\']
  
isHorizontalLine :: Char -> Bool
isHorizontalLine c = c `VU.elem` horizontalLineElements
  where
    horizontalLineElements = VU.fromList ['-', '=']

isVerticalLine :: Char -> Bool
isVerticalLine c = c `VU.elem` verticalLineElements
  where
    verticalLineElements = VU.fromList [':', '|']

isBullet :: Char -> Bool
isBullet '*' = True
isBullet _ = False


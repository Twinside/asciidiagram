{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid( (<>) )
import qualified Data.Text as T
import Text.AsciiDiagram.Parser
import Text.Groom

test1 :: T.Text
test1 =
    "                                   \n" <>
    "  +------------+                   \n" <>
    "  |            |                   \n" <>
    "  |     /------/                   \n" <>
    "  |     |                          \n" <>
    "  |     |                          \n" <>
    "  +-----/                          \n"

main :: IO ()
main = putStrLn . groom $ parseText test1


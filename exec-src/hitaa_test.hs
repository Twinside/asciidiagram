{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-unused-binds #-}

import Data.Monoid( (<>) )
import qualified Data.Text as T
import Text.AsciiDiagram.Parser
import Text.AsciiDiagram.Reconstructor
import Text.AsciiDiagram.Deduplicator
import Text.Groom

test0 :: T.Text
test0 =
    "+------+\n" <>
    "|      |\n" <>
    "|      |\n" <>
    "+------+\n"

test1 :: T.Text
test1 =
    "/------\\\n" <>
    "|      |\n" <>
    "|      |\n" <>
    "\\------/\n"

test2 :: T.Text
test2 =
    "                   \n" <>
    "  +------------+   \n" <>
    "  |            |   \n" <>
    "  |     /------+   \n" <>
    "  |     |      |   \n" <>
    "  |     |      |   \n" <>
    "  +-----+------/   \n"

test3 :: T.Text
test3 =
    "                   \n" <>
    "  +-*----------+   \n" <>
    "  |            |   \n" <>
    "  |  /-----\\   +   \n" <>
    "  |  |     |   |   \n" <>
    "  |  \\-----/   |   \n" <>
    "  +-----+------/   \n"

analyze :: T.Text -> IO ()
analyze txt = do
  let parsed = parseText txt
      reconstructed =
          reconstruct (anchorMap parsed) $ segmentSet parsed
      dedup = deduplicate reconstructed
  putStrLn "================================="
  putStrLn $ T.unpack txt
  putStrLn "\nParsed:\n-------"
  putStrLn $ groom parsed
  putStrLn "\nReconstructed\n------"
  putStrLn $ groom reconstructed
  putStrLn "\nDeduplicated\n------"
  putStrLn $ groom dedup

testList :: [T.Text]
testList =
  [ test0
  , test1
  , test2
  , test3
  ]

main :: IO ()
main = mapM_ analyze testList


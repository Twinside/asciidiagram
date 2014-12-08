{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid( (<>) )
import qualified Data.Text as T
import Text.AsciiDiagram.Parser
import Text.Groom

test1 :: T.Text
test1 =
    "                   \n" <>
    "  +-*----------+   \n" <>
    "  |            |   \n" <>
    "  |     /------+   \n" <>
    "  |     *      |   \n" <>
    "  |     |      |   \n" <>
    "  +-----+------/   \n"

test2 :: T.Text
test2 =
    "                   \n" <>
    "  +-*----------+   \n" <>
    "  |            |   \n" <>
    "  |  /-----\\   +   \n" <>
    "  |  |     |   |   \n" <>
    "  |  \\-----/   |   \n" <>
    "  +-----+------/   \n"

analyze :: T.Text -> IO ()
analyze txt = do
  let parsed = parseText test1
  putStrLn "================================="
  putStrLn $ T.unpack txt
  putStrLn "\nParsed:\n-------"
  putStrLn . groom $ parsed

main :: IO ()
main = mapM_ analyze [test1, test2]


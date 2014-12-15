{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-unused-binds #-}

import Control.Applicative( (<$>) )
import Control.Monad( foldM )
import Data.Monoid( (<>) )
import qualified Data.Text as T
import Text.AsciiDiagram.Parser
import Text.AsciiDiagram.Reconstructor
import Text.AsciiDiagram.Deduplicator
import Text.AsciiDiagram.SvgRender
import Text.Groom
import Graphics.Svg

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

testSmall :: T.Text
testSmall =
    "+++++\n" <>
    "+++++\n" <>
    "+++++\n" <>
    "+++++\n"

testMedium :: T.Text
testMedium =
    "++++++++++\n" <>
    "++++++++++\n" <>
    "++++++++++\n" <>
    "++++++++++\n" <>
    "++++++++++\n"

test4 :: T.Text
test4 =
    "+++++++++++++++\n" <>
    "+++++++++++++++\n" <>
    "+++++++++++++++\n" <>
    "+++++++++++++++\n" <>
    "+++++++++++++++\n" <>
    "+++++++++++++++\n" <>
    "+++++++++++++++\n" <>
    "+++++++++++++++\n"

test5 :: T.Text
test5 =
    " ++++++++++++++\n" <>
    "+++++++++++++++\n" <>
    "+++++++++++++++\n" <>
    "+++++ +++++++++\n" <>
    "+++++++++++++++\n" <>
    "++++++++++ ++++\n" <>
    "+++++++++++++++\n" <>
    "++++++++++++++\n"

analyze :: T.Text -> IO ()
analyze txt = do
  let parsed = parseText txt
      reconstructed =
          reconstruct (anchorMap parsed) $ segmentSet parsed
      deduped = removeLargeCycle reconstructed
  putStrLn "================================="
  putStrLn $ T.unpack txt
  putStrLn "\nParsed:\n-------"
  putStrLn $ groom parsed
  putStrLn "\nReconstructed\n------"
  putStrLn $ groom reconstructed
  putStrLn "\nDeduped\n------"
  putStrLn $ groom deduped

tag :: String -> ShowS -> ShowS
tag tagName content =
    ('<':) . (tagName++) . ('>':) .
        content . 
    ("</" ++) . (tagName++) . ('>':)

html, body, pre :: ShowS -> ShowS
pre = tag "pre"
body = tag "body"
html = tag "html"

img :: String -> ShowS
img href = ("<img src=\""++) . (href++) . ("\" />"++)

toSvg :: [(String, T.Text)] -> IO ()
toSvg lst = do
    hDoc <- html . body <$> foldM go id lst
    writeFile "test.html" $ hDoc ""
  where
    go acc (name, content) = do
      let parsed = parseText content
          reconstructed =
              reconstruct (anchorMap parsed) $ segmentSet parsed
          deduped = removeLargeCycle reconstructed
          fileName = name ++ ".svg"
      saveXmlFile fileName $ shapesToSvgDocument deduped
      return $ acc . img fileName . pre (T.unpack content ++)

testList :: [(String, T.Text)]
testList =
  [ ("t0", test0)
  , ("t1", test1)
  , ("t2", test2)
  , ("t3", test3)
  , ("tsmall", testSmall)
  , ("tmedium", testMedium)
  {-, ("t4", test4)-}
  {-, ("t5", test5)-}
  ]

main :: IO ()
main = do
  mapM_ (analyze . snd) testList
  toSvg testList


{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-unused-binds #-}

import Control.Monad( foldM )
import Data.Monoid( (<>), mempty )
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TIO
import Text.AsciiDiagram.Parser
import Text.AsciiDiagram.Reconstructor
import Text.AsciiDiagram.SvgRender
import Text.Groom
import Graphics.Svg
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import Text.Blaze.Html.Renderer.Text

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
    "  +--*---------+   \n" <>
    "  |            |   \n" <>
    "  |            |   \n" <>
    "  |  /-----\\   +   \n" <>
    "  |  |     |   |   \n" <>
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

toSvg :: [(String, T.Text)] -> IO ()
toSvg lst = do
    hDoc <- foldM go mempty lst
    let html = renderHtml . H.html $ H.body hDoc
    TIO.writeFile "test.html" html
  where
    {-go :: Html -> (String, T.Text) -> IO Html-}
    go acc (name, content) = do
      let parsed = parseText content
          reconstructed =
              reconstruct (anchorMap parsed) $ segmentSet parsed
          {-deduped = removeLargeCycle reconstructed-}
          fileName = name ++ ".svg"
      putStrLn "================================="
      putStrLn $ T.unpack content
      putStrLn "\nParsed:\n-------"
      putStrLn $ groom parsed
      putStrLn "\nReconstructed\n------"
      putStrLn $ groom reconstructed
      saveXmlFile fileName $ shapesToSvgDocument reconstructed
      return $ acc
            <> H.img H.! H.href (H.toValue fileName)
            <> H.pre (H.toHtml content)

testList :: [(String, T.Text)]
testList =
    [("t0", test0)] <>
    [("t1", test1)] <>
    [("t2", test2)] <>
    [("t3", test3)] <>
    [("tsmall", testSmall)] <>
    [("tmedium", testMedium)] <>
    [("t4", test4)] <>
    [("t5", test5)] <>
    []

main :: IO ()
main = toSvg testList


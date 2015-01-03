{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

import Control.Applicative( (<$>) )
import Control.Monad( foldM, forM )
import Data.Monoid( (<>), mempty )
import qualified Data.Text as T
import qualified Data.Text.IO as STIO
import qualified Data.Text.Lazy.IO as TIO
import System.Directory( getDirectoryContents
                       , createDirectoryIfMissing )
import System.FilePath( (</>) )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import Text.Blaze.Html.Renderer.Text

import Text.AsciiDiagram.Parser
import Text.AsciiDiagram.Reconstructor
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

testBadEnd :: T.Text
testBadEnd =
    "/---\\\n" <>
    "|   |\n" <>
    "\\---/\n"

circleEnd :: T.Text
circleEnd =
    "/---\\\n" <>
    "\\---/\n"

verticalCircleEnd :: T.Text
verticalCircleEnd =
    "/\\\n" <>
    "||\n" <>
    "||\n" <>
    "\\/\n"

testBadEndStraight :: T.Text
testBadEndStraight =
    "+---+\n" <>
    "|   |\n" <>
    "+---+\n"

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

testOutputFolder :: FilePath
testOutputFolder = "test_output"

toSvg :: [(String, T.Text)] -> IO ()
toSvg lst = do
    createDirectoryIfMissing True testOutputFolder
    hDoc <- foldM go mempty lst
    let html = renderHtml . H.html $ H.body hDoc
    TIO.writeFile (testOutputFolder </> "test.html") html
  where
    go acc (name, content) = do
      let parsed = parseText content
          reconstructed =
              reconstruct (anchorMap parsed) $ segmentSet parsed
          fileName = name ++ ".svg"
          tlines = T.lines content
          width = maximum $ fmap T.length tlines
      putStrLn name

      -- {-
      putStrLn "================================="
      putStrLn $ T.unpack content
      putStrLn "\nParsed:\n-------"
      putStrLn $ groom parsed
      putStrLn "\nReconstructed\n------"
      putStrLn $ groom reconstructed
      -- -}

      saveXmlFile (testOutputFolder </> fileName) $
          shapesToSvgDocument (width, length tlines) reconstructed
      return $ acc
            <> H.img H.! H.src (H.toValue fileName)
            <> H.pre (H.toHtml content)

testList :: [(String, T.Text)]
testList =
    [("circleEnd", circleEnd)] <>
    [("verticalCircleEnd", verticalCircleEnd)] <>
    [("tbadend", testBadEnd)] <>
    [("testBadEndStraight", testBadEndStraight)] <>
    [("t0", test0)] <>
    [("t1", test1)] <>
    [("t2", test2)] <>
    [("t3", test3)] <>
    [("tsmall", testSmall)] <>
    [("tmedium", testMedium)] <>
    [("t4", test4)] <>
    [("t5", test5)] <>
    []

loadTests :: IO [(String, T.Text)]
loadTests = do
  let folder = "tests" </> "text"
  content <-
      filter (`notElem` [".", ".."]) <$> getDirectoryContents folder
  forM content $ \f ->
     (f,) <$> STIO.readFile (folder </> f)

main :: IO ()
main = do
  tests <- loadTests 
  toSvg $ testList ++ tests
  {-toSvg [("testBadEndStraight", testBadEndStraight)]-}


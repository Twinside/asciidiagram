{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid( mempty )
#endif

import Control.Applicative( (<$>) )
import Control.Monad( foldM, forM )
import Data.Monoid( (<>) )
import Data.List( sort )
import qualified Data.Text as T
import qualified Data.Text.IO as STIO
import qualified Data.Text.Lazy.IO as TIO
import System.Directory( getDirectoryContents
                       , createDirectoryIfMissing )
import System.FilePath( (</>) )
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import Text.Blaze.Html.Renderer.Text

import Graphics.Rasterific.Svg( renderSvgDocument
                              , loadCreateFontCache )

import Codec.Picture( writePng )
import Text.AsciiDiagram
import Text.Groom
import Graphics.Svg

testOutputFolder :: FilePath
testOutputFolder = "test_output"

toSvg :: [(String, T.Text)] -> IO ()
toSvg lst = do
    createDirectoryIfMissing True testOutputFolder
    cache <- loadCreateFontCache "asciidiagram-fonty-fontcache"
    (hDoc, _) <- foldM go (mempty, cache) lst
    let html = renderHtml . H.html $ H.body hDoc
    TIO.writeFile (testOutputFolder </> "test.html") html
  where
    go (acc, cache) (name, content) = do
      let diagram = parseAsciiDiagram content
          fileName = name ++ ".svg"
          pngname = name ++ ".png"
          svgDoc = svgOfDiagram diagram
      putStrLn name
      putStrLn $ groom diagram

      saveXmlFile (testOutputFolder </> fileName) svgDoc
      (img, _) <- renderSvgDocument cache Nothing 96 svgDoc
      writePng (testOutputFolder </> pngname) img
      return . (, cache) $ acc
            <> H.table
                  (H.tr $ H.td (H.img H.! H.src (H.toValue fileName))
                       <> H.td (H.img H.! H.src (H.toValue pngname)))
            <> H.pre (H.toHtml (name ++ "\n") <> H.toHtml content)

loadTests :: IO [(String, T.Text)]
loadTests = do
  let folder = "tests" </> "text"
  content <- -- return ["art3.txt"]
     sort . filter (`notElem` [".", "", ".."]) <$> getDirectoryContents folder
  forM content $ \f ->
     (f,) <$> STIO.readFile (folder </> f)

main :: IO ()
main = do
  tests <- loadTests 
  toSvg tests


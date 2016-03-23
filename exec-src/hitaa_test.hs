{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>) )
import Data.Monoid( mempty )
#endif

import Control.Monad( foldM, forM )
import Data.Monoid( (<>) )
import Data.List( isSuffixOf, sort )
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
{-import Text.Groom-}
import Graphics.Svg

testOutputFolder :: FilePath
testOutputFolder = "test_output"

loadLibrary :: Maybe FilePath -> IO Document
loadLibrary filePath = case filePath of
   Nothing -> return defaultLib
   Just p -> loadLib p
  where
    defaultLib = defaultLibrary defaultGridSize
    loadLib p = do                      
      putStrLn $ "Loading shape lib " ++ p
      f <- loadSvgFile p
      case f of
        Just doc -> return doc
        Nothing -> do
          putStrLn "Invalid library file, using default lib"
          return defaultLib 

toSvg :: [(String, T.Text, Maybe FilePath)] -> IO ()
toSvg lst = do
    createDirectoryIfMissing True testOutputFolder
    cache <- loadCreateFontCache "asciidiagram-fonty-fontcache"
    (hDoc, _) <- foldM go (mempty, cache) lst
    let html = renderHtml . H.html $ H.body hDoc
    TIO.writeFile (testOutputFolder </> "test.html") html
  where
    go (acc, cache) (name, content, lib) = do
      shapeLib <- loadLibrary lib                      
      let diagram = parseAsciiDiagram content
          fileName = name ++ ".svg"
          pngname = name ++ ".png"
          svgDoc = svgOfDiagramAtSize defaultGridSize shapeLib diagram
      putStrLn name
      {-putStrLn $ show diagram-}

      saveXmlFile (testOutputFolder </> fileName) svgDoc
      (img, _) <- renderSvgDocument cache Nothing 96 svgDoc
      writePng (testOutputFolder </> pngname) img
      return . (, cache) $ acc
            <> H.table
                  (H.tr $ H.td (H.img H.! H.src (H.toValue fileName))
                       <> H.td (H.img H.! H.src (H.toValue pngname)))
            <> H.pre (H.toHtml (name ++ "\n") <> H.toHtml content)

loadTests :: IO [(String, T.Text, Maybe FilePath)]
loadTests = do                                   
  let folder = "tests" </> "text"
  content <- -- return ["shaper.txt"]
     sort . filter (".txt" `isSuffixOf`)
          . filter (`notElem` [".", "", ".."]) <$> getDirectoryContents folder
  forM content $ \f -> do
    fileContent <- STIO.readFile (folder </> f)
    let contentLines = T.lines fileContent
        libMarker = "|LIB_TO_LOAD="
    case contentLines of
      [] -> return (f, fileContent, Nothing)
      (x:xs) | libMarker `T.isPrefixOf` x ->
         return (f, T.unlines xs, (folder </>) . T.unpack <$> T.stripPrefix libMarker x)
      _ -> return (f, fileContent, Nothing)

main :: IO ()
main = do
  tests <- loadTests 
  toSvg tests


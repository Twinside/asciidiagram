{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>), (<*>), pure )
#endif

import Control.Applicative( (<|>) )
import Control.Monad( when )
import Data.Monoid( (<>) )

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Directory( getTemporaryDirectory )
import System.FilePath( (</>)
                      , replaceExtension
                      , takeExtension )

import Graphics.Svg( Document, loadSvgFile, saveXmlFile )
import Graphics.Rasterific.Svg( loadCreateFontCache )
import Graphics.Text.TrueType( FontCache )
import Codec.Picture( writePng )
import Options.Applicative( Parser
                          , ParserInfo
                          , argument
                          , execParser
                          , flag
                          , fullDesc
                          , header
                          , help
                          , helper
                          , info
                          , long
                          , metavar
                          , optional
                          , progDesc
                          , short
                          , str
                          , strOption
                          , switch
                          )
import Graphics.Rasterific.Svg( renderSvgDocument
                              , pdfOfSvgDocument )
import Text.AsciiDiagram

data Mode
  = Convert !(FilePath, FilePath)
  | DumpLibrary !FilePath

data Options = Options
  { _workingMode :: !Mode
  , _verbose     :: !Bool
  , _withLibrary :: !(Maybe FilePath)
  , _format      :: !(Maybe Format)
  }

data Format = FormatSvg | FormatPng | FormatPdf

ioParser :: Parser (String, String)
ioParser = (,)
    <$> argument str
          (metavar "INPUTFILE"
          <> help "Text file of the Ascii diagram to parse.")
    <*> (argument str
            (metavar "OUTPUTFILE"
            <> help ("Output file name, same as input with"
                    <> " different extension if unspecified."))
        <|> pure "")

modeParser :: Parser Mode
modeParser = (Convert <$> ioParser) <|> (DumpLibrary <$> dumpParser)
  where 
    dumpParser = strOption 
      ( long "dump-library"
      <> short 'd'
      <> metavar "FILENAME"
      <> help "Dump the default shape library & styles in an SVG document") 

argParser :: Parser Options
argParser = Options
  <$> modeParser
  <*> ( switch (long "verbose" <> help "Display more information") )
  <*> ( optional $ strOption
            ( long "with-library"
            <> short 'l'
            <> metavar "LIBRARY_FILENAME"
            <> help "Use a custom shape & style library instead of the default one") )
  <*> ( flag Nothing (Just FormatSvg)
            (  long "svg"
            <> help "Force the use of the SVG format (deduced from extension otherwise)")
     <|> flag Nothing (Just FormatPng)
            ( long "png"
            <> help "Force the use of the PNG format (deduced from extension otherwise) (by default)")
     <|> flag Nothing (Just FormatPdf)
            ( long "pdf"
            <> help  "Force the use of the PDF format (deduced from extension otherwise)")
      )

progOptions :: ParserInfo Options
progOptions = info (helper <*> argParser)
      ( fullDesc
     <> progDesc "Convert INPUTFILE into a svg or png OUTPUTFILE"
     <> header "asciidiagram - A pretty printer for ASCII art diagram to SVG." )

formatOfOuputFilename :: FilePath -> Format
formatOfOuputFilename f = case takeExtension f of
    ".png" -> FormatPng
    ".svg" -> FormatSvg
    ".pdf" -> FormatPdf
    _ -> FormatPng

getFontCache :: Bool -> IO FontCache
getFontCache verbose = do
  when verbose $ putStrLn "Loading/Building font cache (can be long)"
  tempDir <- getTemporaryDirectory 
  loadCreateFontCache $ tempDir </> "asciidiagram-fonty-fontcache"


loadLibrary :: Options -> IO Document
loadLibrary opt = case _withLibrary opt of
   Nothing -> return defaultLib
   Just p -> loadLib p
  where
    defaultLib = defaultLibrary defaultGridSize
    loadLib p = do
      f <- loadSvgFile p
      case f of
        Just doc -> return doc
        Nothing -> do
          putStrLn "Invalid library file, using default lib"
          return defaultLib 

runConversion :: Options -> FilePath -> FilePath -> IO ()
runConversion opt inputFile outputFile = do
  verbose . putStrLn $ "Loading file " ++ inputFile
  inputData <- T.decodeUtf8 <$> B.readFile inputFile
  lib <- loadLibrary opt
  let diag = parseAsciiDiagram inputData
      svg = svgOfDiagramAtSize defaultGridSize lib diag
      format = _format opt <|> (pure $ formatOfOuputFilename outputFile)
  case format of
    Nothing -> saveDoc svg
    Just FormatSvg -> saveDoc svg
    Just FormatPng -> savePng svg
    Just FormatPdf -> savePdf svg
  where
    verbose = when $ _verbose opt
    saveDoc svg = do
      verbose . putStrLn $ "Writing SVG file " ++ outputFile
      saveXmlFile (savingPath "svg") svg

    savingPath ext = case outputFile of
      "" -> replaceExtension inputFile ext
      p -> p

    savePdf svg = do
      cache <- getFontCache  $ _verbose opt
      verbose . putStrLn $ "Writing PDF file " ++ outputFile
      (pdf, _) <- pdfOfSvgDocument cache Nothing 96 svg
      LB.writeFile (savingPath "pdf") pdf

    savePng svg = do
      cache <- getFontCache  $ _verbose opt
      verbose . putStrLn $ "Writing PNG file " ++ outputFile
      (img, _) <- renderSvgDocument cache Nothing 96 svg
      writePng (savingPath "png") img

dumpLibrary :: FilePath -> IO ()
dumpLibrary path =
  saveXmlFile path $ defaultLibrary defaultGridSize

main :: IO ()
main = execParser progOptions >>= \opts ->
  case _workingMode opts of
    Convert (i, o) -> runConversion opts i o
    DumpLibrary p -> dumpLibrary p


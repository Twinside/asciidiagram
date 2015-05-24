{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<$>), (<*>), pure )
#endif

import Control.Applicative( (<|>) )
import Control.Monad( when )
import Data.Monoid( (<>) )

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.IO as STIO
import System.FilePath( replaceExtension
                      , takeExtension )

import Graphics.Rasterific.Svg( loadCreateFontCache )

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
                          , progDesc
                          , str
                          , switch
                          )
import Text.AsciiDiagram

data Options = Options
  { _inputFile  :: !FilePath
  , _outputFile :: !FilePath
  , _verbose    :: !Bool
  , _format     :: !(Maybe Format)
  }

data Format = FormatSvg | FormatPng | FormatPdf

argParser :: Parser Options
argParser = Options
  <$> ( argument str
            (metavar "INPUTFILE"
            <> help "Text file of the Ascii diagram to parse."))
  <*> ( argument str
            (metavar "OUTPUTFILE"
            <> help ("Output file name, same as input with"
                    <> " different extension if unspecified."))
        <|> pure "" )
  <*> ( switch (long "verbose" <> help "Display more information") )
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

runConversion :: Options -> IO ()
runConversion opt = do
  verbose . putStrLn $ "Loading file " ++ _inputFile opt
  inputData <- STIO.readFile $ _inputFile opt
  let diag = parseAsciiDiagram inputData
      format = _format opt <|> (pure . formatOfOuputFilename $ _outputFile opt)
  case format of
    Nothing -> saveDoc diag
    Just FormatSvg -> saveDoc diag
    Just FormatPng -> savePng diag
    Just FormatPdf -> savePdf diag
  where
    verbose = when $ _verbose opt
    saveDoc diag = do
      verbose . putStrLn $ "Writing SVG file " ++ _outputFile opt
      saveAsciiDiagramAsSvg (savingPath "svg") diag

    savingPath ext = case _outputFile opt of
      "" -> replaceExtension (_inputFile opt) ext
      p -> p

    savePdf diag = do
      verbose . putStrLn $ "Loading/Building font cache (can be long)"
      cache <- loadCreateFontCache "asciidiagram-fonty-fontcache"
      verbose . putStrLn $ "Writing PDF file " ++ _outputFile opt
      pdf <- pdfOfDiagram cache 96 diag
      LB.writeFile (savingPath "pdf") pdf

    savePng diag = do
      verbose . putStrLn $ "Loading/Building font cache (can be long)"
      cache <- loadCreateFontCache "asciidiagram-fonty-fontcache"
      verbose . putStrLn $ "Writing PNG file " ++ _outputFile opt
      img <- imageOfDiagram cache 96 diag
      writePng (savingPath "png") img

main :: IO ()
main = execParser progOptions >>= runConversion


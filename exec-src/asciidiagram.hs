{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative( (<*>), pure )
#endif

import Control.Applicative( (<$>), (<|>) )
import Control.Monad( when )
import Data.Monoid( (<>) )

import qualified Data.Text.IO as STIO
import System.FilePath( replaceExtension
                      , takeExtension )

import Graphics.Rasterific.Svg( renderSvgDocument
                              , loadCreateFontCache )

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
import Graphics.Svg

data Options = Options
  { _inputFile  :: !FilePath
  , _outputFile :: !FilePath
  , _verbose    :: !Bool
  , _format     :: !(Maybe Format)
  }

data Format = FormatSvg | FormatPng

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
    _ -> FormatPng

runConversion :: Options -> IO ()
runConversion opt = do
  verbose . putStrLn $ "Loading file " ++ _inputFile opt
  inputData <- STIO.readFile $ _inputFile opt
  let svgDoc = svgOfDiagram $ parseAsciiDiagram inputData
  case (_format opt, formatOfOuputFilename $ _outputFile opt) of
    (Nothing, FormatSvg) -> saveDoc svgDoc
    (Just FormatSvg, _) -> saveDoc svgDoc
    (Nothing, FormatPng) -> savePng svgDoc
    (Just FormatPng, _) -> savePng svgDoc
  where
    verbose = when $ _verbose opt
    saveDoc doc = do
      verbose . putStrLn $ "Writing SVG file " ++ _outputFile opt
      saveXmlFile (savingPath "svg") doc

    savingPath ext = case _outputFile opt of
      "" -> replaceExtension (_inputFile opt) ext
      p -> p

    savePng doc = do
      verbose . putStrLn $ "Loading/Building font cache (can be long)"
      cache <- loadCreateFontCache "asciidiagram-fonty-fontcache"
      verbose . putStrLn $ "Writing PNG file " ++ _outputFile opt
      (img, _) <- renderSvgDocument cache Nothing 96 doc
      writePng (savingPath "png") img

main :: IO ()
main = execParser progOptions >>= runConversion


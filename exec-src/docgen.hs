
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
import Control.Applicative( (<$>) )
import Data.Monoid( (<>) )
import qualified Data.Text as T
import qualified Data.Text.IO as STIO
import System.Directory( createDirectoryIfMissing )
import System.FilePath( (</>) )

import Text.AsciiDiagram
import Codec.Picture( writePng )
import Graphics.Svg
import Graphics.Rasterific.Svg( renderSvgDocument
                              , loadCreateFontCache )

docOutputFolder :: FilePath
docOutputFolder = "docimages"

toSvg :: [(String, T.Text)] -> IO ()
toSvg lst = do
    createDirectoryIfMissing True docOutputFolder
    mapM_ go lst
  where
    go (name, content) = do
      let fileName = name ++ ".svg"
          svgDoc = svgOfDiagram $ parseAsciiDiagram content
      saveXmlFile (docOutputFolder </> fileName) svgDoc

toPng :: [(String, T.Text)] -> IO ()
toPng lst = do
    createDirectoryIfMissing True docOutputFolder
    cache <- loadCreateFontCache "asciidiagram-fonty-fontcache"
    mapM_ (go cache) lst
  where
    go cache (name, content) = do
      let fileName = name ++ ".png"
          svgDoc = svgOfDiagram $ parseAsciiDiagram content
      (img, _) <- renderSvgDocument cache Nothing 96 svgDoc
      writePng (docOutputFolder </> fileName) img

simpleLines :: T.Text
simpleLines =
  " -----       \n" <>
  "   -------   \n" <>
  "             \n" <>
  " |  |        \n" <>
  " |  |        \n" <>
  " |  \\----    \n" <>
  " |           \n" <>
  " +-----      \n"

dashedLines :: T.Text
dashedLines =
  " -----       \n" <>
  "   -=-----   \n" <>
  "             \n" <>
  " |  :        \n" <>
  " |  |        \n" <>
  " |  \\----    \n" <>
  " |           \n" <>
  " +--=--      \n"

arrows :: T.Text
arrows =
  "     ^\n" <>
  "     |\n" <>
  "     |\n" <>
  "<----+---->\n" <>
  "     |  < > v ^\n" <>
  "     |\n" <>
  "     v\n" <>
  ""

complexClosed :: T.Text
complexClosed =
  "  +------+\n" <>
  "  |      |\n" <>
  "  |      +--+\n" <>
  "  |      |  |\n" <>
  "  +---+--+  |\n" <>
  "      |     |\n" <>
  "      +-----+\n"

dashingClosed :: T.Text
dashingClosed =
  "  +--+  +--+  +=-+  +=-+\n" <>
  "  |  |  :  |  |  |  |  :\n" <>
  "  +--+  +--+  +--+  +-=+\n"

curvedCorner :: T.Text
curvedCorner =
  "  /--+  +--\\  +--+  /--+\n" <>
  "  |  |  |  |  |  |  |  |\n" <>
  "  +--+  +--+  \\--+  +--+\n" <>
  "\n" <>
  "  /--+  /--\\  /--+  /--\\ .\n" <>
  "  |  |  |  |  |  |  |  |\n" <>
  "  +--/  +--+  \\--/  +--/\n" <>
  "\n" <>
  "  /--\\ .\n" <>
  "  |  |\n" <>
  "  \\--/\n" <>
  ".\n"

bulletTest :: T.Text
bulletTest =
  "  *-*-*\n" <>
  "  |   |  *----*\n" <>
  "  +---/       |\n" <>
  "          * * *\n"

styleExample :: T.Text
styleExample =
  " +--------+         +--------+\n" <>
  " | Source +-------->| op1    |\n" <>
  " | {src}  |         \\---+----/\n" <>
  " +--------+             |\n" <>
  "            +-------*<--/\n" <>
  " +------+<--| op2   |\n" <>
  " | Dest |   +-------+\n" <>
  " |{dst} |\n" <>
  " +------+\n" <>
  "\n" <>
  "::: .src { fill: #AAF; }\n" <>
  "::: .dst { stroke: #FAA; stroke-width: 3px; }\n"

cabalExample :: T.Text
cabalExample =
  "                /---------+\n" <>
  "+---------+     |         |\n" <>
  "|  ASCII  +---->| Diagram |\n" <>
  "+---------+     |         |\n" <>
  "|         |     +--+------/\n" <>
  "\\---*-----/<=======/\n"

baseExample :: T.Text
baseExample =
  "                /---------+\n" <>
  "+---------+     |         |\n" <>
  "|  ASCII  +---->| Diagram |\n" <>
  "+---------+     |         |\n" <>
  "|{flat}   |     +--+------/\n" <>
  "\\---*-----/<=======/\n" <>
  "::: .flat { fill: #DDD; }"

data Doc
  = P      T.Text
  | Schema FilePath T.Text
 
toHaddock :: [Doc] -> [T.Text]
toHaddock = concatMap go where
  go (P t) = T.lines t ++ [""]
  go (Schema n v) =
      ["", "@"]
        ++ (cleanupHaddock <$> T.lines v)
        ++ ["@"
           , "<<" <> T.pack docOutputFolder <> "/" <> T.pack n <> ".svg>>"
           ,""
           ]

linesDoc :: [Doc]
linesDoc =
  [P ("The basic syntax of asciidiagrams is made of lines made out\\n" <>
      "of \\'-\\' and \\'|\\' characters. They can be connected with anchors\\n" <>
      "like \\'+\\' (direct connection) or \\'\\\\\\' and \\'\\/\\' (smooth connections)\\n"
     )
  ,Schema "simple_lines" simpleLines
  ,P ("You can use dashed lines by using ':' for vertical lines or '=' for\\n" <>
      "horizontal lines.")
  ,Schema "dashed_lines" dashedLines
  ,P ("Arrows are made out of the \\'\\<\\', \\'\\>\\', \\'^\\' and \\'v\\'\\n"<>
      "characters.\\n" <>
      "If the arrows are not connected to any lines, the text is left as is.\\n")
  ,Schema "arrows" arrows
  ]

shapesDoc :: [Doc]
shapesDoc =
  [P ("If the lines are closed, then it is detected as such and rendered\n" <>
      "differently\n")
  ,Schema "complexClosed" complexClosed 
  ,P ("If any of the segment posess one of the dashing markers (\\':\\' or \\'=\\')\n" <>
      "Then the full shape will be dashed.\n")
  ,Schema "dashingClosed" dashingClosed 
  ,P ("Any of the angle of a shape can curved one of the smooth corner anchor\n" <>
      "(\\'\\\\\\' or \\'\\/\\')\n")
  ,Schema "curvedCorner" curvedCorner 
  ]

bulletDoc :: [Doc]
bulletDoc =
  [P ("Adding a \\'*\\' on a line or on a shape add a little circle on it.\n" <>
      "If the bullet is not attached to any shape or lines, then it\n" <>
      "will be render like any other text.\n")
  ,Schema "bulletTest" bulletTest
  ,P ("When used at connection points, it behaves like the \\'+\\' anchor.\n")
  ]

styleDoc :: [Doc]
styleDoc =
  [P ("The shapes can ba annotated with a tag like `{tagname}`.\n" <>
      "Tags will be inserted in the class attribute of the shape\n" <>
      "and can then be stylized with a CSS.\n")
  ,Schema "styleExample" styleExample
  ,P ("Inline css styles are introduced with the \":::\" prefix\n" <>
      "at the beginning of the line. They are introduced in the\n" <>
      "style section of the generated CSS file")
  ,P ("The generated geometry also possess some predefined class\n" <>
      "which are overidable:\n" <>
      "\n" <>
      " * \"dashed_elem\" is applied on every dashed element.\n" <>
      "\n" <>
      " * \"filled_shape\" is applied on every closed shape.\n" <>
      "\n" <>
      " * \"arrow_head\" is applied on arrow head.\n" <>
      "\n" <>
      " * \"bullet\" on every bullet placed on a shape or line.\n" <>
      "\n" <>
      " * \"line_element\" on every line element, this include the arrow head.\n" <>
      "\n" <>
      "You can then customize the appearance of the diagram as you want.\n")
  ]

introDoc :: [Doc]
introDoc =
  [ P ("Ascii diagram, transform your ASCII art drawing to a nicer\n" <>
       "representation")
  , Schema "baseExample" baseExample
  ]

doc :: [(T.Text, [Doc])]
doc =
  [("cabal", [Schema "cabalSchema" cabalExample])
  ,("introDoc", introDoc)
  ,("linesdoc", linesDoc)
  ,("shapesdoc", shapesDoc)
  ,("bulletdoc", bulletDoc)
  ,("styledoc", styleDoc)
  ]
    
cleanupHaddock :: T.Text -> T.Text
cleanupHaddock = T.concatMap escape where
  escape '<' = "\\<"
  escape '>' = "\\>"
  escape '\'' = "\\'"
  escape '\\' = "\\\\"
  escape '/' = "\\/"
  escape c = T.singleton c

toHaskellFile :: [(T.Text, [T.Text])] -> IO ()
toHaskellFile docs =
  STIO.writeFile "src/Text/AsciiDiagram/doc.hs" . T.unlines $ concatMap hsFile docs
  where
    hsFile (tag, lns) = "": ("-- $" <> tag) : fmap prepare lns
    prepare t = "-- " <> t

main :: IO ()
main = do
  toSvg [(f, c) | Schema f c <- concatMap snd doc]
  toPng [(f, c) | Schema f c <- concatMap snd doc]
  toHaskellFile $ [(t, toHaddock d)  | (t, d) <- doc]


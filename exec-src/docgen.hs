
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
simpleLines = T.unlines
  [ " -----       "
  , "   -------   "
  , "             "
  , " |  |        "
  , " |  |        "
  , " |  \\----    "
  , " |           "
  , " +-----      "
  ]

dashedLines :: T.Text
dashedLines = T.unlines
  [ " -----       "
  , "   -=-----   "
  , "             "
  , " |  :        "
  , " |  |        "
  , " |  \\----    "
  , " |           "
  , " +--=--      "
  ]

arrows :: T.Text
arrows = T.unlines
  [ "     ^"
  , "     |"
  , "     |"
  , "<----+---->"
  , "     |  < > v ^"
  , "     |"
  , "     v"
  , ""
  ]

complexClosed :: T.Text
complexClosed = T.unlines
  [ "  +------+"
  , "  |      |"
  , "  |      +--+"
  , "  |      |  |"
  , "  +---+--+  |"
  , "      |     |"
  , "      +-----+"
  ]

dashingClosed :: T.Text
dashingClosed = T.unlines
  [ "  +--+  +--+  +=-+  +=-+"
  , "  |  |  :  |  |  |  |  :"
  , "  +--+  +--+  +--+  +-=+"
  ]

curvedCorner :: T.Text
curvedCorner = T.unlines
  [ "  /--+  +--\\  +--+  /--+"
  , "  |  |  |  |  |  |  |  |"
  , "  +--+  +--+  \\--+  +--+"
  , ""
  , "  /--+  /--\\  /--+  /--\\ ."
  , "  |  |  |  |  |  |  |  |"
  , "  +--/  +--+  \\--/  +--/"
  , ""
  , "  /--\\ ."
  , "  |  |"
  , "  \\--/"
  , "."
  ]

bulletTest :: T.Text
bulletTest = T.unlines
  [ "  *-*-*"
  , "  |   |  *----*"
  , "  +---/       |"
  , "          * * *"
  ]

styleExample :: T.Text
styleExample = T.unlines
  [ " +--------+         +--------+"
  , " | Source +-------->| op1    |"
  , " | {src}  |         \\---+----/"
  , " +--------+             |"
  , "            +-------*<--/"
  , " +------+<--| op2   |"
  , " | Dest |   +-------+"
  , " |{dst} |"
  , " +------+"
  , ""
  , "::: .src .filled_shape { fill: #AAF; }"
  , "::: .dst .filled_shape { stroke: #FAA; stroke-width: 3px; }"
  ]

cabalExample :: T.Text
cabalExample = T.unlines
  [ "                /---------+"
  , "+---------+     |         |"
  , "|  ASCII  +---->| Diagram |"
  , "+---------+     |         |"
  , "|         |     +--+------/"
  , "\\---*-----/<=======/"
  ]

baseExample :: T.Text
baseExample = T.unlines
  [ "                /---------+"
  , "+---------+     |         |"
  , "|  ASCII  +---->| Diagram |"
  , "+---------+     |         |"
  , "| {flat}  |     +--+------/"
  , "\\---*-----/<=======/"
  , "::: .flat .filled_shape { fill: #DED; }"
  ]

shapeExample :: T.Text
shapeExample = T.unlines
  [ " +---------+  +----------+"
  , " |         |  |          |"
  , " | circle  |  |          |"
  , " |         |  |    io    |"
  , " |{circle} |  | {io}     |"
  , " +---------+  +----------+"
  , ""
  , " +----------+  +----------+"
  , " |document  |  |          |"
  , " |          |  |          |"
  , " |          |  | storage  |"
  , " |{document}|  | {storage}|"
  , " +----------+  +----------+"
  , ""
  , "::: .circle .filled_shape { shape: circle; }"
  , "::: .document .filled_shape { shape: document; }"
  , "::: .storage .filled_shape { shape: storage; }"
  , "::: .io .filled_shape { shape: io; }"
  ]

deepStyleExample :: T.Text
deepStyleExample = T.unlines
  [ " /------------------------------------------------------\\ ."
  , " |s100                                                  |"
  , " |    /----------------------------\\                    |"
  , " |    |s1         /--------\\       |  e1    /--------\\  |"
  , " |    |      *--->|  s2    |       +------->|  s10   |  |"
  , " |    +----+      \\---+----/       |        \\--------/  |"
  , " |    | i4 |          |            |           ^        |"
  , " |    |{ii}+---------\\| e1  {lo}   |           |        |"
  , " |    +----+         vv            | ealarm    |        |   e0      /-------------\\ ."
  , " |    |            /--------\\      +-----------/        +---------->|    s50      |"
  , " |    +----\\       | s3 {lu}|      |                    |           \\-------------/"
  , " |    | o5 |   e2  \\--+-----/      |                    |"
  , " |    |{oo}|<---------/            |<-\\                 |"
  , " |    \\-+--+--------------------+--/  |                 |"
  , " |      |                       |     | eReset          |"
  , " |      |                       \\-----/                 |"
  , " |      v                                               |"
  , " |  /--------\\                                          |"
  , " |  |  s20   |                  {li}                    |"
  , " |  \\--------/                                          |"
  , " \\------------------------------------------------------/"
  , ""
  , "::: .li .line_element { stroke: purple; }"
  , "::: .li .arrow_head, .li text { fill: gray; }"
  , "::: .lo .line_element { stroke: blue; }"
  , "::: .lo .arrow_head, .lo text { fill: green; }"
  , "::: .lu .line_element { stroke: red; }"
  , "::: .lu .arrow_head, .lu text { fill: orange; }"
  , "::: .ii .filled_shape { fill: #DDF; }"
  , "::: .ii text { fill: blue; }"
  , "::: .oo .filled_shape { fill: #DFD; }"
  , "::: .oo text { fill: pink; }"
  ]

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

pp :: [T.Text] -> Doc
pp = P . T.unlines

linesDoc :: [Doc]
linesDoc =
  [pp [ "The basic syntax of asciidiagrams is made of lines made out"
      , "of \\'-\\' and \\'|\\' characters. They can be connected with anchors"
      , "like \\'+\\' (direct connection) or \\'\\\\\\' and \\'\\/\\' (smooth connections)"
      ]
  ,Schema "simple_lines" simpleLines
  ,pp ["You can use dashed lines by using ':' for vertical lines or '=' for"
      ,"horizontal line"]
  ,Schema "dashed_lines" dashedLines
  ,pp [ "Arrows are made out of the \\'\\<\\', \\'\\>\\', \\'^\\' and \\'v\\'"
      , "characters."
      , "If the arrows are not connected to any lines, the text is left as is."
      ]
  ,Schema "arrows" arrows
  ]

shapesDoc :: [Doc]
shapesDoc =
  [pp [ "If the lines are closed, then it is detected as such and rendered"
      , "differently"
      ]
  ,Schema "complexClosed" complexClosed 
  ,pp [ "If any of the segment posess one of the dashing markers (\\':\\' or \\'=\\')"
      , "Then the full shape will be dashed."
      ]
  ,Schema "dashingClosed" dashingClosed 
  ,pp [ "Any of the angle of a shape can curved one of the smooth corner anchor"
      , "(\\'\\\\\\' or \\'\\/\\')"
      ]
  ,Schema "curvedCorner" curvedCorner 
  ]

bulletDoc :: [Doc]
bulletDoc =
  [pp [ "Adding a \\'*\\' on a line or on a shape add a little circle on it."
      , "If the bullet is not attached to any shape or lines, then it"
      , "will be render like any other text."
      ]
  ,Schema "bulletTest" bulletTest
  ,pp ["When used at connection points, it behaves like the \\'+\\' anchor."]
  ]

styleDoc :: [Doc]
styleDoc =
  [P $ T.unlines
     ["The shapes can ba annotated with a tag like `{tagname}`."
     ,"Tags will be inserted in the class attribute of the shape"
     ,"and can then be stylized with a CSS."
     ]
  ,Schema "styleExample" styleExample
  ,P $ T.unlines
     ["Inline css styles are introduced with the \":::\" prefix"
     ,"at the beginning of the line. They are introduced in the"
     ,"style section of the generated CSS file"
     ]
  ,P $ T.unlines
     ["The generated geometry also possess some predefined class"
     ,"which are overidable:"
     ,""
     ," * \"dashed_elem\" is applied on every dashed element."
     ,""
     ," * \"filled_shape\" is applied on every closed shape."
     ,""
     ," * \"arrow_head\" is applied on arrow head."
     ,""
     ," * \"bullet\" on every bullet placed on a shape or line."
     ,""
     ," * \"line_element\" on every line element, this include the arrow head."
     ,""
     ,"You can then customize the appearance of the diagram as you want."
     ]
  ]

hierarchicalDoc :: [Doc]
hierarchicalDoc =
   [pp [ "Starting with version 1.3, all shapes, text and lines are"
       , "hierachised, a shape within a shape will be integrated within"
       , "the same group. This allows more complex styling: "
       ]
   ,Schema "deepStyleExample" deepStyleExample
   ,pp [ "In the previous example, we can see that the lines color are"
       , "'shape scoped' and the tag applied to the shape above them"
       , "applies to them"
       ]
   ]

shapeDoc :: [Doc]
shapeDoc =
   [pp [ "From version 1.3, you can substitute the shape of your element"
       , "with one from a shape library. Right now the shape library is"
       , "relatively small:"
       ]
   ,Schema "shapeExample" shapeExample
   ,pp [ "The mechanism use CSS styling to change the shape, if a CSS rule"
       , "possess a `shape` pseudo attribute, then the generated shape is replaced"
       , "with a SVG `use` tag with the value of the shape attribute as `href`"
       ]
   ,pp [ "But, you can create your own style library and change the default"
       , "stylesheet. You can retrieve the default one with the shell command"
       , "`asciidiagram --dump-library default-lib.svg`"
       , ""
       , "You can then add your own symbols tag in it and use it by calling"
       , "`asciidiagram --with-library your-lib.svg`."
       ]
   ]

introDoc :: [Doc]
introDoc =
  [ pp ["Ascii diagram, transform your ASCII art drawing to a nicer"
       , "representation"
       ]
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
  ,("hierarchicalDoc", hierarchicalDoc)
  ,("shapeDoc", shapeDoc)
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


import Graphics.Svg( applyCSSRules
                   , xmlOfDocument
                   , Document( _styleRules )  )
import Text.Pandoc.JSON( Block( CodeBlock, RawBlock )
                       , Format( .. )
                       , toJSONFilter )
import Text.AsciiDiagram( parseAsciiDiagram, svgOfDiagram )
import Text.XML.Light.Output( ppcTopElement, prettyConfigPP )
import qualified Data.Text as T

main :: IO ()
main = toJSONFilter insertDiagrams

insertDiagrams :: Block -> IO [Block]
insertDiagrams (CodeBlock (_ident, classes, _attrs) code)
  | "ditaa" `elem` classes || "asciidiagram" `elem` classes = do
     let svg = applyCSSRules . svgOfDiagram . parseAsciiDiagram $ T.pack code
         stripCss d = d { _styleRules = [] }
         xmlStr = ppcTopElement prettyConfigPP . xmlOfDocument $ stripCss svg
     return [RawBlock (Format "html") $ xmlStr]
insertDiagrams block = return [block]


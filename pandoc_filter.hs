import Control.Applicative
import System.Directory (createDirectory, doesDirectoryExist)
import System.FilePath ((<.>), (</>))
import System.IO
import Graphics.Svg
import Text.Pandoc.JSON
import Text.AsciiDiagram
import Text.XML.Light.Output( ppcTopElement, prettyConfigPP )
import qualified Data.Text as T

main :: IO ()
main = toJSONFilter insertDiagrams

insertDiagrams :: Block -> IO [Block]
insertDiagrams (CodeBlock (ident, classes, attrs) code)
  | "ditaa" `elem` classes || "asciidiagram" `elem` classes = do
     let svg = xmlOfDocument . svgOfDiagram . parseAsciiDiagram $ T.pack code
         xmlStr = ppcTopElement prettyConfigPP svg
     return [RawBlock (Format "html") $ xmlStr]
insertDiagrams block = return [block]

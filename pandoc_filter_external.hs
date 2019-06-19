import Data.Monoid( (<>) )
import qualified Data.ByteString.Char8 as B
import Data.IORef( IORef, newIORef, readIORef, writeIORef )
import Text.Pandoc.JSON( Block( CodeBlock, RawBlock, Plain )
                       , Inline( Image )
                       , nullAttr
                       , toJSONFilter )
import Text.AsciiDiagram( parseAsciiDiagram, saveAsciiDiagramAsSvg )
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Debug.Trace

main :: IO ()
main = do
    counter <- newIORef 0
    toJSONFilter (insertDiagrams counter)

insertDiagrams :: IORef Int -> Block -> IO [Block]
insertDiagrams counter (CodeBlock (_ident, classes, _attrs) code)
  | "ditaa" `elem` classes || "asciidiagram" `elem` classes = do
     c <- readIORef counter
     writeIORef counter (c + 1)
     let path = ("diagrams/_" <> show c <> ".svg")
     saveAsciiDiagramAsSvg path . parseAsciiDiagram $ T.pack code
     let img = Image nullAttr [] (path, "")
     return [Plain [img]]
insertDiagrams _ block = return [block]


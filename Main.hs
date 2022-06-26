module Main where

import Control.Arrow
import Data.ByteString (hGetContents)
import Data.Default
import Data.Either
import Data.Functor
import Data.Text.Encoding
import Prelude hiding (hGetContents, putStr, lines, unlines, concat)
import Data.Text (pack, concat, lines, unlines, Text)
import Data.Text.IO (putStr)
import System.Environment
import System.IO (openFile, IOMode(..), stdin)
import Text.Pandoc.Class
import Text.Pandoc.Error
import Text.Pandoc.Logging
import Text.Pandoc.Options
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Writers.HTML

mkSource name = (>>= hGetContents) >>> fmap (decodeUtf8 >>> (name,))

setSource (path:_) = mkSource path (openFile path ReadMode)
setSource _ = mkSource "<standard input>" (return stdin)

shouldReport NoTranslation{} = False
shouldReport CouldNotLoadTranslations{} = False
shouldReport msg = messageVerbosity msg == WARNING

logPretty msg =
  case lines (showLogMessage msg) of
    [] -> [] 
    l:ls -> concat ["[", verbosity msg, "] ", l] : map ("\t"<>) ls
      where verbosity = pack . show . messageVerbosity

doPandoc :: (FilePath, Text) -> PandocPure Text
doPandoc source =
  readMarkdown def{readerExtensions = pandocExtensions} [source]
  >>= writeHtml5String def{writerHTMLMathMethod = MathML}
  >>= \t -> getLog <&> filter shouldReport <&> \case
    [] -> t
    ms ->
      unlines (t:"<pre class=\"warning\">":concatMap logPretty ms)
      <> "</pre>\n"

doError = flip either id $
    renderError >>> (:["</pre>"]) >>> ("<pre class=\"error\">":) >>> unlines

main :: IO ()
main = getArgs >>= setSource <&> (doPandoc >>> runPure >>> doError) >>= putStr

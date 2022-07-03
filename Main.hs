module Main where

import Prelude hiding (putStr, lines, unlines, log)
import Control.Monad (join, (>=>))
import Data.Bool
import Data.Char (toLower)
import Data.ByteString (hGetContents)
import Data.Default
import Data.Functor ((<&>))
import Data.Maybe (maybeToList)
import Data.Monoid (Sum(..), Alt(..))
import Data.String (fromString)
import Data.Text (Text, pack, lines, unlines)
import Data.Text.Encoding
import Data.Text.IO (putStr)
import Data.Text.Lazy (toStrict)
import System.Console.GetOpt
import System.Environment
import System.IO (openFile, IOMode(..), stdin)
import Text.Blaze.Html5 (pre, toHtml, (!))
import Text.Blaze.Html5.Attributes (class_)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Pandoc
import Text.Pandoc.Shared
import Text.Pandoc.Sources (toSources)
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Shared

(.>) = flip (.)

(..>) :: (a -> b -> c) -> (c -> d) -> a -> b -> d
op ..> f = op .> (.> f)

(>&>) :: Monad m => (a -> m b) -> (b -> c) -> a -> m c
k >&> f = k >=> f .> return

(>^>) :: Monad m => m a -> (a -> b -> c) -> b -> m c
m >^> op = (<$> m) . flip op

infixl 2 >&>, >^>

readMd = readCommonMark def{
  readerExtensions = extensionsFromList [
    Ext_raw_html,
    Ext_gfm_auto_identifiers, Ext_implicit_header_references, Ext_footnotes,
    Ext_autolink_bare_uris,
    Ext_pipe_tables,
    Ext_tex_math_dollars
  ],
  readerAbbreviations = mempty
}

writeHtml = writeHtml5String def{
  writerHTMLMathMethod = MathML,
  writerHighlightStyle = Nothing,
  writerSyntaxMap = mempty
}

getMd :: [String] -> IO (PandocPure Pandoc)
getMd = getSource >&> (:[]) .> toSources .> readMd where
  getSource (path:_) = mkSource path         =<< openFile path ReadMode
  getSource       [] = mkSource "standard input" stdin
  mkSource = (,) ..> mapM (hGetContents >&> decodeUtf8)

mapBlocks :: ([Block] -> [Block]) -> Pandoc -> Pandoc
mapBlocks f (Pandoc meta bs) = Pandoc meta (f bs)

worthToc :: [Block] -> Bool
worthToc = query countHeaders .> \(h1s, h2s) -> h1s > 1 || h2s > 3 where
  countHeaders (Header 1 _ _) = (1, 0)
  countHeaders (Header 2 _ _) = (0, 1)
  countHeaders _ = mempty :: (Sum Int, Sum Int)

mkToc :: [Block] -> Block
mkToc = toTableOfContents def .> \case
  BulletList [[Plain _, bl@(BulletList _)]] -> bl -- don't link to a solitary h1
  other -> other

addToc :: Pandoc -> Pandoc
addToc = mapBlocks $ join (worthToc .> bool id (mkToc >>= (:)))

care :: LogMessage -> Bool
care NoTranslation{} = False
care CouldNotLoadTranslations{} = False
care msg = messageVerbosity msg >= WARNING

addLog :: [LogMessage] -> Text -> Text
addLog [] = id
addLog ms = (<> htmlLog WARNING showLogMessage ms)

htmlLog :: Verbosity -> (msg -> Text) -> [msg] -> Text
htmlLog level render = concatMap (render .> lines .> pretty) .> unlines .> inPre
  where pretty [] = []
        pretty (l:ls) = "[" <> tshow level <> "] " <> l : map ("\t"<>) ls
        inPre = toHtml .> (pre ! class__ level) .> renderHtml .> toStrict
        class__ = show .> map toLower .> fromString .> class_

fmtHtml :: PandocPure Pandoc -> PandocPure Text
fmtHtml = (=<<) $ addToc .> writeHtml >=> getLog >^> filter care .> addLog

htmlOut :: PandocPure Pandoc -> Text
htmlOut = fmtHtml .> runPure .> either ((:[]) .> htmlLog ERROR renderError) id

-- write title as plain text to strip `code` etc
titleOut :: PandocPure Pandoc -> Text
titleOut = (>>= toTitle .> writePlain def) .> runPure .> either (const "") id
  where toTitle = mapBlocks (query (join $ h1 ..> Alt) .> getAlt .> maybeToList)
        h1 = \case Header 1 _ _ -> Just; _ -> const Nothing

dispatch :: [String] -> IO Text
dispatch = getOpt RequireOrder [t] .> \case
  ( _,  _, es@(_:_)) -> return $ htmlLog ERROR pack es
  (os, as,        _) -> getMd as <&> case os of [] -> htmlOut; _ -> titleOut
  where t = Option "t" [] (NoArg ()) "extract title instead of formatting"

main :: IO ()
main = getArgs >>= dispatch >>= putStr

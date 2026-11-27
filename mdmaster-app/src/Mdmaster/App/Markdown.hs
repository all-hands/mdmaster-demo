module Mdmaster.App.Markdown
  ( parseMarkdown
  , renderMarkdown
  , renderMarkdownFor
  , frontMatter
  ) where

import Data.Functor.Identity (runIdentity)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Text.Pandoc
  ( Extension (Ext_startnum, Ext_yaml_metadata_block)
  , Extensions
  , ReaderOptions (..)
  , WrapOption (WrapPreserve)
  , WriterOptions (..)
  , def
  , enableExtension
  , githubMarkdownExtensions
  , readMarkdown
  , renderError
  , runPure
  , writeMarkdown
  )
import Text.Pandoc.Definition (Block (..), Format (..), Inline (..), Meta (..), Pandoc (..), nullMeta)
import Text.Pandoc.Walk (walk)
import Text.Pandoc.Templates (Template, compileTemplate)

dialect :: Extensions
dialect =
  enableExtension Ext_startnum
    (enableExtension Ext_yaml_metadata_block githubMarkdownExtensions)

readerOptions :: ReaderOptions
readerOptions = def { readerExtensions = dialect }

writerOptions :: WriterOptions
writerOptions = def
  { writerExtensions = dialect
  , writerWrapText   = WrapPreserve
  }

parseMarkdown :: Text -> Either Text Pandoc
parseMarkdown input =
  case runPure (readMarkdown readerOptions input) of
    Left err  -> Left (renderError err)
    Right doc -> Right doc

renderMarkdown :: Pandoc -> Either Text Text
renderMarkdown = renderWith . walk protectTrailingHashes

renderMarkdownFor :: Text -> Pandoc -> Pandoc -> Either Text Text
renderMarkdownFor source (Pandoc parsedMeta _) doc@(Pandoc meta blocks) =
  case frontMatter source of
    Just block | meta == parsedMeta -> do
      body <- renderMarkdown (Pandoc nullMeta blocks)
      pure (if T.null (T.strip body) then block else block <> "\n" <> body)
    _ -> renderMarkdown doc

frontMatter :: Text -> Maybe Text
frontMatter source = case T.lines (T.replace "\r\n" "\n" source) of
  open : rest
    | T.stripEnd open == "---"
    , (inside, close : _) <- break (\l -> T.stripEnd l `elem` ["---", "..."]) rest ->
        Just (T.unlines (open : inside <> [close]))
  _ -> Nothing

protectTrailingHashes :: Block -> Block
protectTrailingHashes block = case block of
  Header lvl attr inlines -> Header lvl attr (fixLast inlines)
  _                       -> block
  where
    fixLast inlines = case reverse inlines of
      Str t : earlier
        | "#" `T.isSuffixOf` t ->
            let body = T.dropWhileEnd (== '#') t
                hashes = T.takeWhileEnd (== '#') t
             in reverse earlier
                  <> [Str body | not (T.null body)]
                  <> [RawInline (Format "markdown") (T.replicate (T.length hashes) "\\#")]
      _ -> inlines

renderWith :: Pandoc -> Either Text Text
renderWith doc@(Pandoc (Meta meta) _) = do
  template <-
    if M.null meta
      then pure Nothing
      else Just <$> gfmTemplate
  case runPure (writeMarkdown writerOptions { writerTemplate = template } doc) of
    Left err  -> Left (renderError err)
    Right out -> Right (T.stripEnd out <> "\n")

gfmTemplate :: Either Text (Template Text)
gfmTemplate =
  case runIdentity (compileTemplate "templates/default.commonmark" templateSource) of
    Left err -> Left ("Error compiling template templates/default.commonmark: "
                        <> T.pack err)
    Right t  -> Right t

templateSource :: Text
templateSource = T.unlines
  [ "$if(titleblock)$"
  , "$titleblock$"
  , ""
  , "$endif$"
  , "$for(header-includes)$"
  , "$header-includes$"
  , ""
  , "$endfor$"
  , "$for(include-before)$"
  , "$include-before$"
  , ""
  , "$endfor$"
  , "$if(toc)$"
  , "$table-of-contents$"
  , ""
  , "$endif$"
  , "$body$"
  , "$for(include-after)$"
  , ""
  , "$include-after$"
  , "$endfor$"
  ]

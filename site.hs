------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import Text.Pandoc.Options
import Text.Pandoc.Definition
import Text.Pandoc.Walk (walk, walkM)
import Text.Pandoc.Highlighting (pygments)
import Debug.Trace (traceShowId)

import qualified Text.HTML.TagSoup as TS (parseTags, renderTags, Tag(TagOpen))

import Control.Monad ((>=>))

import Data.ByteString.Lazy.Char8 (pack, unpack, ByteString)
import qualified Network.URI.Encode as URI (encode)
import qualified Data.Text as Text

import Data.String (fromString)

import Data.Functor ((<&>))



--------------------------------------------------------------------------------

config :: Configuration
config = defaultConfiguration
  { destinationDirectory = "_site"
  }

readerOptions = defaultHakyllReaderOptions
writerOptions = defaultHakyllWriterOptions
  { writerHighlightStyle   = Just pygments
  , writerHTMLMathMethod = MathJax ""
  }

myPandocCompiler =
  pandocCompilerWithTransformM readerOptions writerOptions $ walkM tikzFilter

main :: IO ()
main = hakyllWith config $ do
    match "CNAME" $ do
        route idRoute
        compile copyFileCompiler

    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "files/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["index.md", "about.md", "writing.md"]) $ do
        route   $ setExtension "html"
        compile $ myPandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ myPandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["blog.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let blogCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Blog"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/blog.html" blogCtx
                >>= loadAndApplyTemplate "templates/default.html" blogCtx
                >>= relativizeUrls

--    match "index.html" $ do
--        route idRoute
--        compile $ do
--            posts <- recentFirst =<< loadAll "posts/*"
--            let indexCtx =
--                    listField "posts" postCtx (return posts) `mappend`
--                    constField "title" "Home"                `mappend`
--                    defaultContext
--
--            getResourceBody
--                >>= applyAsTemplate indexCtx
--                >>= loadAndApplyTemplate "templates/default.html" indexCtx
--                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

tikzFilterWith :: FilePath -> SizeOption -> Block -> Compiler Block
tikzFilterWith templatename size (CodeBlock (id, classes, namevals) contents) =
 --(imageBlock . ("data:image/svg+xml;utf8," ++) . URI.encode . filter (/= '\n') . itemBody <$>) $
  makeItem (Text.unpack contents)
    >>= loadAndApplyTemplate (fromFilePath templatename) (bodyField "body")
    <&> itemBody
    <&> pack
    >>= unixFilterLBS "rubber-pipe" ["--pdf"]
    >>= unixFilterLBS "pdftocairo" ["-svg", "-", "-"]
    <&> unpack
    <&> filter (/= '\n')
    <&> imageBlockFromSVG size id classes namevals
tikzFilterWith _ _ x = return x

tikzFilter :: Block -> Compiler Block
tikzFilter cb@(CodeBlock (_, "tikzpicture":_, _) _) = tikzFilterWith "templates/tikzpicture.tex" FreeSize cb
tikzFilter cb@(CodeBlock (_, "tikzcd":_, _) _) = tikzFilterWith "templates/tikzcd.tex" (FixedScale 0.2 "em") cb
tikzFilter x = return x

data SizeOption = FreeSize | FixedScale Float String

imageBlockFromSVG :: SizeOption -> Text.Text -> [Text.Text] -> [(Text.Text,Text.Text)] -> String -> Block
imageBlockFromSVG size id classes namevals code =
        let dataurl = "data:image/svg+xml;utf8," ++ (URI.encode code)
            tags = TS.parseTags code
            findSvgTagAttrs [] = error "malformed SVG"
            findSvgTagAttrs (TS.TagOpen "svg" attrs : _) = attrs
            findSvgTagAttrs (_:rest) = findSvgTagAttrs rest
            sizeAttrs = [(name,val) | (name,val) <- findSvgTagAttrs tags, name `elem` ["height", "width"]]
            resize scale unit val = let (no,_):_ = reads val
                                     in show (scale * no :: Float) ++ unit
            extraAttrs = case size of
                          FreeSize -> []
                          FixedScale scale unit -> [(fromString name, fromString (resize scale unit val)) | (name,val) <- sizeAttrs]
         in Para [Image (id, classes, extraAttrs ++ namevals) [] (fromString dataurl, "")]
 


--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import Text.Pandoc.Options
import Text.Pandoc.Highlighting (pygments)


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
        compile $ pandocCompilerWith readerOptions writerOptions
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompilerWith readerOptions writerOptions
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


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext


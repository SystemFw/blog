--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid ((<>))
import           Hakyll
import           Text.Pandoc


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "static/*/*" $ do
        route idRoute
        compile copyFileCompiler

    match "content/about.md" $ do
        route contentRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" siteCtx
            >>= loadAndApplyTemplate "templates/default.html" siteCtx
            >>= relativizeUrls

    match "content/posts/*" $ do
        route contentRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls
          
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            archiveCtx <- allPosts
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["index.html"] $ do
        route idRoute
        compile $ do
            indexCtx <- allPosts
            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html" indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    siteCtx

siteCtx :: Context String
siteCtx =
    constField "baseurl" "http://localhost:8000" <>
    constField "site_description" "SystemFw FP blog" <>
    constField "linkedin_username" "fabiolabella" <>
    constField "github_username" "SystemFw" <>
    defaultContext

contentRoute :: Routes
contentRoute = gsubRoute "content/" (const "") `composeRoutes` setExtension "html"

allPosts :: Compiler (Context String)
allPosts = do
    posts <- loadAll "content/posts/*" >>= recentFirst
    pure $ listField "posts" postCtx (pure posts) <>
           siteCtx

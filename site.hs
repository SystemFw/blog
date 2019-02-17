--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


import           Data.Monoid ((<>))
import           Hakyll
import           Text.Pandoc
import qualified Data.Aeson as Yaml
import qualified Data.Aeson.Types as Yaml
import           Data.Aeson ((.:), (.:?), (.!=))
import qualified Data.Text as Text
import           Data.String

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "static/*/*" $ do
        route idRoute
        compile copyFileCompiler

    match "content/about.md" $ do
        route contentRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/page.html" site
            >>= loadAndApplyTemplate "templates/default.html" site
            >>= relativizeUrls

    match "content/posts/*" $ do
        route contentRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" post
            >>= loadAndApplyTemplate "templates/default.html" post
            >>= relativizeUrls
          
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" allPosts
                >>= loadAndApplyTemplate "templates/default.html" allPosts
                >>= relativizeUrls

    create ["index.html"] $ do
        route idRoute
        compile $
          makeItem ""
              >>= loadAndApplyTemplate "templates/index.html" allPosts
              >>= loadAndApplyTemplate "templates/default.html" allPosts
              >>= relativizeUrls

    create ["talks.html"] $ do
      compile $ getMetadata "content/talks.md" >> makeItem ()
  
    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------

post :: Context String
post =
    dateField "date" "%B %e, %Y" <>
    site

site :: Context String
site =
    constField "baseurl" "http://localhost:8000" <>
    constField "site_description" "SystemFw FP blog" <>
    constField "linkedin_username" "fabiolabella" <>
    constField "github_username" "SystemFw" <>
    defaultContext

contentRoute :: Routes
contentRoute = gsubRoute "content/" (const "") `composeRoutes` setExtension "html"

allPosts :: Context String
allPosts =
  listField "posts" post (loadAll "content/posts/*" >>= recentFirst) <>
  site

----------------------------------------------------------------------------------

yo :: Compiler Yaml.Object
yo = do
  md <- getMetadata "content/talks.md"
  pure md

talk :: Yaml.Object -> Yaml.Parser (Context String)
talk t = do
  let fetch ctx name = fmap (ctx name) (t .: Text.pack name)
  fields <- traverse (fetch constField) ["title", "video", "year", "conf"]
  slides :: Maybe String <- t .:? "slides"
  pure $ maybe mempty (constField "slides") slides <> 
         mconcat fields <>
         site
  


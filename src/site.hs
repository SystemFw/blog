--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Monoid ((<>))
import           Data.Bifunctor (first)
import           Control.Applicative (empty)
import           Control.Monad.Except (liftEither)
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
      route idRoute
      compile $
        makeItem ""
              >>= loadAndApplyTemplate "templates/talks.html" talks
              >>= loadAndApplyTemplate "templates/default.html" talks
              >>= relativizeUrls

    create ["writings.html"] $ do
      route idRoute
      compile $
        makeItem ""
              >>= loadAndApplyTemplate "templates/writings.html" writings
              >>= loadAndApplyTemplate "templates/default.html" writings
              >>= relativizeUrls


    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
contentRoute :: Routes
contentRoute = gsubRoute "content/" (const "") `composeRoutes` setExtension "html"

site :: Context String
site =
    constField "baseurl" "http://localhost:8000" <>
    constField "site_description" "SystemFw FP blog" <>
    constField "linkedin_username" "fabiolabella" <>
    constField "github_username" "SystemFw" <>
    defaultContext

post :: Context String
post =
    dateField "date" "%B %e, %Y" <>
    site

allPosts :: Context String
allPosts =
  listField "posts" post (loadAll "content/posts/*" >>= recentFirst) <>
  site

talks :: Context String
talks = listField "talks" talk talkMetadata <> site
 where
   talkMetadata :: Compiler [Item Yaml.Object]
   talkMetadata = do
       md <- getMetadata "content/talks.md"
       items <- yamlParser (\o -> o .: "talks") md
       traverse makeItem items

talk :: Context Yaml.Object
talk =
  foldMap mandatoryField ["title", "video", "conf"] <>
  optionalField "slides" <>
  numberField "year"


writings :: Context String
writings = listField "writings" writing writingMetadata <> site
 where
   writingMetadata :: Compiler [Item Yaml.Object]
   writingMetadata = do
       md <- getMetadata "content/writings.md"
       items <- yamlParser (\o -> o .: "writings") md
       traverse makeItem items

writing :: Context Yaml.Object
writing = foldMap  mandatoryField ["title", "link"]

yamlField name runParse parser =
  field name $ \item -> runParse parser $ itemBody item

mandatoryField name =
  yamlField name yamlParser $ \o -> o .: Text.pack name

numberField name = yamlField name yamlParser $ \o -> do
  n :: Integer <- o .: Text.pack name
  pure $ show n

optionalField name = yamlField name
   (\parser o -> maybe empty pure =<< yamlParser parser o)
   (\o -> o .:? Text.pack name)

yamlParser :: (Yaml.Object -> Yaml.Parser a) -> Yaml.Object -> Compiler a
yamlParser p = \o -> liftEither . first pure $ Yaml.parseEither p o

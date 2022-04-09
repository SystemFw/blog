--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Monoid ((<>))
import           Data.Bifunctor (first)
import           Control.Applicative (empty)
import           Control.Monad.Except (liftEither)

import           Hakyll
import           Text.Pandoc
import           Data.String

import qualified Data.Aeson as Yaml
import qualified Data.Aeson.Types as Yaml
import           Data.Aeson ((.:), (.:?), (.!=))


import Data.Text(Text)

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match ("CNAME" .||. "static/**") $ do
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
        compile $ noHighlightingCompiler
            >>= loadAndApplyTemplate "templates/page.html" post
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

noHighlightingCompiler :: Compiler (Item String)
noHighlightingCompiler =
  pandocCompilerWith
     defaultHakyllReaderOptions
   $ defaultHakyllWriterOptions { writerHighlightStyle = Nothing }


contentRoute :: Routes
contentRoute = gsubRoute "content/" (const "") `composeRoutes` setExtension "html"

site :: Context String
site =
    constField "site_description" "SystemFw FP blog" <>
    constField "linkedin_username" "fabiolabella" <>
    constField "github_username" "SystemFw" <>
    defaultContext

post :: Context String
post =
    dateField "date" "%B %e, %Y" <>
    constField "syntax_style" "zenburn" <>
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
  foldMap mandatoryField ["title", "conf", "id"] <>
  foldMap optionalField ["video", "slides"] <>
  numberField "year"


writings :: Context String
writings = listField "writings" writing writingMetadata <> site
 where
   writingMetadata :: Compiler [Item Yaml.Object]
   writingMetadata = do
       md <- getMetadata "content/writings.md"
       items <- yamlParser (\o -> o .: "writings") md
       traverse makeItem $ reverse items

writing :: Context Yaml.Object
writing = foldMap  mandatoryField ["title", "link"]

mandatoryField :: String -> Context Yaml.Object
mandatoryField name =
  yamlField name yamlParser $ \o -> o .: fromString name

numberField :: String -> Context Yaml.Object
numberField name = yamlField name yamlParser $ \o -> do
  n :: Integer <- o .: fromString name
  pure $ show n

optionalField :: String -> Context Yaml.Object
optionalField name = yamlField name
   (\parser o -> maybe empty pure =<< yamlParser parser o)
   (\o -> o .:? fromString name)


yamlField :: String -> (a -> b -> Compiler String) -> a -> Context b
yamlField name runParse parser =
  field name $ \item -> runParse parser $ itemBody item

yamlParser :: (Yaml.Object -> Yaml.Parser a) -> Yaml.Object -> Compiler a
yamlParser p = \o -> liftEither . first pure $ Yaml.parseEither p o

#!/usr/bin/env cabal

{- cabal:
build-depends:
  base >= 4.18 && < 4.20,
  filepath ^>= 1.4.100.1,
  hakyll >= 4.16 && < 4.17,
-}
{-# LANGUAGE OverloadedStrings #-}

import Data.String (String)
import Hakyll
import System.FilePath (dropExtension, takeDirectory, takeFileName, (</>))

main :: IO ()
main = hakyll $ do
  match ("images/**" .||. "gallery/**" .||. "style.css") $ do
    route idRoute
    compile copyFileCompiler

  match "blog/**" $ do
    route cleanRoute
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/post.html" blogPostContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext

  match "index.md" $ do
    route $ setExtension "html"
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext

  match "installing.md" $ do
    route cleanRoute
    compile $
      pandocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext

  match "blog.html" $ do
    route cleanRoute
    compile $
      getResourceBody
        >>= applyAsTemplate blogListContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext

  match "gallery.html" $ do
    route cleanRoute
    compile $ do
      images <- loadAll "gallery/*.png"
      imgTpl <- loadBody "templates/gallery-image.html"
      imgs <- applyTemplateList imgTpl defaultContext images

      let galleryCtx = constField "images" imgs <> defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/gallery.html" galleryCtx
        >>= loadAndApplyTemplate "templates/default.html" galleryCtx

  match "templates/*" $ compile templateBodyCompiler

cleanRoute :: Routes
cleanRoute = customRoute $ (</> "index.html") . dropExtension . toFilePath

blogPostContext :: Context String
blogPostContext =
  dateField "date" "%B %e, %Y"
    <> mapContext stripIndex (urlField "url")
    <> defaultContext
 where
  stripIndex url
    | takeFileName url == "index.html" = takeDirectory url
    | otherwise = url

blogListContext :: Context String
blogListContext =
  listField "posts" blogPostContext (recentFirst =<< loadAll "blog/*")
    <> defaultContext

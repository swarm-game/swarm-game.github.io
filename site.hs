#!/usr/bin/env cabal

{- cabal:
build-depends:
  base ^>= 4.18.0.0,
  filepath ^>= 1.4.100.1,
  hakyll ^>= 4.16.1.0,
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
    compile $
      getResourceBody
        >>= applyAsTemplate galleryContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext

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

galleryContext :: Context String
galleryContext =
  listField "images" defaultContext (loadAll "gallery/*")
    <> defaultContext

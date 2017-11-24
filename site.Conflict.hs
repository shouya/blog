--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid         (mappend, (<>))
import           System.FilePath     ((</>), takeDirectory, takeBaseName)
import           Data.List           (isSuffixOf)
import           Data.Maybe          (maybe, fromJust, fromMaybe)
import           Debug.Trace         (trace)
import           Control.Monad       (liftM)
import           Control.Applicative (empty)

import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    tags <- buildTags "posts/*" (fromCapture "tags/*/index.html")
    tagsRules tags $ \tag pattern -> do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx = constField "title" ("Posts tagged with \"" ++ tag ++ "\"")
                  `mappend` listField "posts" postCtx (return posts)
                  `mappend` defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/tag.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    cats <- buildTags "posts/*" (fromCapture "categories/*/index.html")
    tagsRules cats $ \tag pattern -> do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll pattern
        let ctx = constField "title" ("Posts categorized as \"" ++ tag ++ "\"")
                  `mappend` listField "posts" postCtx (return posts)
                  `mappend` defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/category.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    let taggedPostCtx = tagListContext cats "categories" <>
                        tagListContext tags "tags" <>
                        postCtx

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
          posts <- fmap (take 5) . recentFirst =<<
            loadAllSnapshots "posts/*" "content"
          let indexCtx = listField "posts" postCtx (return posts) <>
                         constField "title" "Home"                <>
                         boolField "notitle" (const True)         <>
                         defaultContext

          getResourceBody
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/default.html" indexCtx
            >>= relativizeUrls
            >>= cleanIndexUrls

    create ["archive.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
        let ctx = listField "posts" postCtx (return posts) <>
                  constField "title" "Archive"             <>
                  defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/archive.html" ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls
          >>= cleanIndexUrls


    match "posts/*" $ do
        route blogPostRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" taggedPostCtx
            >>= cleanIndexUrls
            >>= saveSnapshot "extern-content"
            >>= relativizeUrls
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= cleanIndexUrls
            >>= relativizeUrls


    match "templates/*" $ compile templateBodyCompiler

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx <> bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
          loadAllSnapshots "posts/*" "extern-content"
        renderAtom myFeedConfiguration feedCtx posts


--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    visibleCtx <>
    teaserField "teaser" "content" <>
    defaultContext

visibleCtx :: Context String
visibleCtx = field "visible" $ \(Item ident _) -> do
  cond <- fromMaybe "true" <$> getMetadataField ident "published"
  if cond == "true" then pure "true" else empty


---- Route

blogPostRoute :: Routes
blogPostRoute = gsubRoute "posts/" (const "blog/")
                `composeRoutes`
                gsubRoute "[0-9]{4}-[0-9]{1,2}-[0-9]{1,2}-" (const "")
                `composeRoutes`
                cleanRoute

cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
          where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
      pattern = "/index.html"
      replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
  { feedTitle = "Shou's origin"
  , feedDescription = "Source of everything"
  , feedAuthorName = "Shou Ya"
  , feedAuthorEmail = "log AT lain.li"
  , feedRoot        = "https://log.lain.li"
  }

listContextWith :: Context String -> String -> Context a
listContextWith ctx s = listField s ctx $ do
  ident <- getUnderlying
  metaStr <- getMetadataField ident s
  let metas = maybe [] (map trim . splitAll ",") metaStr
  return $ map (\x -> Item (fromFilePath x) x) metas

tagListContext :: Tags -> String -> Context a
tagListContext tx s = listField s tagContext $ do
  tags <- getUnderlying >>= getTags
  return $ map (\x -> Item (tagsMakeId tx x) x) tags
  where tagContext = tagName <> tagLink
        tagName    = bodyField "name"
        tagLink    = urlField "link"

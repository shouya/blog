--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, (<>))
import           Hakyll
import           System.FilePath ((</>), takeDirectory, takeBaseName)
import           Data.List (isSuffixOf)
import           Data.Maybe (maybe)

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

    let taggedPostCtx = postCtx

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ blogPostRoute
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" taggedPostCtx
            >>= cleanIndexUrls
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "templates/*" $ compile templateBodyCompiler

    create ["atom.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx `mappend` bodyField "description"
        posts <- fmap (take 10) . recentFirst =<<
          loadAllSnapshots "posts/*" "content"
        renderAtom myFeedConfiguration feedCtx posts


--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    metadataField <>
    defaultContext

---- Route

blogPostRoute :: Routes
blogPostRoute = gsubRoute "posts/" (const "blog/") `composeRoutes`
                gsubRoute "[0-9]{4}-[0-9}{1,2}[0-9]{1,2}-" (const "") `composeRoutes`
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
  let metas = $ map trim . splitAll "," <$> metaStr
  return $ map (\x -> Item (fromFilePath x) x) metas

--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.ByteString.Lazy (toStrict)
import           Data.Foldable (for_)
import           Data.Semigroup ((<>))
import           Data.Text (unpack)
import           Data.Yaml
import           Eurorack.Synthesizers
import           Hakyll
import           Lucid hiding (for_)

main :: IO ()
main = hakyllWith config $ do
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match (fromList ["about.markdown", "contact.markdown"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

  tags <- buildTags ("posts/**" .||. "eurorack/jams/*") (fromCapture "tags/*.html")
  categories <- buildCategories "posts/**" (fromCapture "categories/*.html")

  match "eurorack/jams/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler >>=
              saveSnapshot "content" >>=
              loadAndApplyTemplate "templates/jam.html" postCtx >>=
              loadAndApplyTemplate "templates/default.html" postCtx >>=
              relativizeUrls

  match "posts/**" $ do
    route $ setExtension "html"
    compile $ pandocCompiler >>=
              saveSnapshot "content" >>=
              loadAndApplyTemplate "templates/post.html" postCtx >>=
              loadAndApplyTemplate "templates/default.html" postCtx >>=
              relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll ("posts/**" .||. "eurorack/jams/*")
      let indexCtx =
              listField "posts" postCtx (return posts) <>
              constField "title" "Home"                <>
              defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

  match "eurorack/*.yaml" $ do
    route $ setExtension "html"
    compile $ rackCompiler
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls

  create ["synth.html"] $ do
    route idRoute
    compile $ do
        makeItem "" >>= applyTemplate synthTemplate postCtx
                    >>= loadAndApplyTemplate "templates/default.html" postCtx
                    >>= relativizeUrls

  for_ [minBound .. maxBound] $ \mod ->
    create [fromFilePath $ unpack $ "eurorack/modules/" <> identifier mod <> ".html"] $ do
      route idRoute
      let ctx = constField "title" (show $ fullName mod) <> postCtx
      compile $ makeItem (show (moduleHtml mod)) >>=
                loadAndApplyTemplate "templates/default.html" ctx >>=
                relativizeUrls

  match "jams.html" $ do
    route idRoute
    compile $ do
      jams <- recentFirst =<< loadAll "eurorack/jams/*"
      let indexCtx =
              listField "jams" postCtx (return jams) <>
              defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  tagsRules categories $ \category pattern -> do
    let title = "Posts in category " <> category
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx = constField "title" title <> listField "posts" postCtx (return posts) <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  tagsRules tags $ \tag pattern -> do
    let title = "Posts with tag " <> tag
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx = constField "title" title <> listField "posts" postCtx (return posts) <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = teaserField "teaser" "content"
       <> dateField "date" "%B %e, %Y"
       <> defaultContext

synthTemplate :: Template
synthTemplate = readTemplate . show $ html where
  html :: Html ()
  html = p_ "Hello"

rackCompiler :: Compiler (Item String)
rackCompiler = getResourceLBS >>= traverse go where
  go yaml = case decodeEither $ toStrict $ yaml :: Either String System of
    Left e -> fail $ show e
    Right v -> if all verify v
               then pure . show $ systemHtml v
               else fail "illegal rack"

verify (Case "A100LMB" rows) = length rows == 2
verify (Case "A100LMS9" rows) = length rows == 3

config = defaultConfiguration {
  deployCommand = "rsync -avcz _site/ mlang@blind.guru:blind.guru/modular/"
}

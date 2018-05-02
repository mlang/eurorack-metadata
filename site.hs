--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.ByteString.Lazy (toStrict)
import           Data.Foldable (for_)
import           Data.Metrology ((#))
import           Data.Metrology.SI (Length)
import           Data.Semigroup ((<>))
import           Data.Text (unpack)
import           Data.Units.SI (Ampere(..), Meter(..))
import           Data.Units.SI.Prefixes (centi, milli)
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
    create ([fromFilePath $ unpack $ "eurorack/modules/" <> identifier mod <> ".markdown"]) $ do
      route $ setExtension "html"
      let ctx = moduleCtx mod
      compile $ pandocCompiler >>=
                loadAndApplyTemplate "templates/module.html" ctx >>=
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

moduleCtx :: Module -> Context String
moduleCtx m = constField "title" (show $ fullName m)
           <> constField "synopsis" (maybe "" show $ synopsis m)
           <> constField "homepage" (maybe "" unpack $ url m)
           <> functionField "current" c
	   <> lengthField "width" (width m)
           <> constField "frontPanel" (show $ frontPanelHtml m)
           <> defaultContext
  where
    c ["+12V", "mA"] i = case currents m of
      Currents mA _ _ -> pure $ show $ round $ mA # milli Ampere
    c ["-12V", "mA"] i = case currents m of
      Currents _ mA _ -> pure $ show $ round $ mA # milli Ampere
    c ["+5V", "mA"] i = case currents m of
      Currents _ _ mA -> pure $ show $ round $ mA # milli Ampere
    c _ i = error $ "current(): missing argument in item " <> show (itemIdentifier i)

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

lengthField :: String -> Length -> Context String
lengthField k l = functionField k f where
  f ["HP"] i = pure $ show $ round $ l # HorizontalPitch
  f ["U"] i = pure $ show $ round $ l # RackUnit
  f [] i = f ["HP"] i
  f _ i = error $ "width(): Unsupported unit in item " <> show (itemIdentifier i)

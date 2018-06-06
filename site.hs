--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative (Alternative(..))
import           Control.Monad (mplus)
import           Data.ByteString.Lazy (toStrict)
import           Data.Foldable (for_)
import           Data.List                       (intercalate, intersperse)
import           Data.Maybe (catMaybes, fromMaybe)
import           Data.Metrology ((#))
import           Data.Metrology.SI (Length)
import           Data.Semigroup ((<>))
import           Data.Text (pack, unpack)
import           Data.Traversable (for)
import           Data.Units.SI (Ampere(..), Meter(..))
import           Data.Units.SI.Prefixes (centi, milli)
import           Data.Yaml
import           Eurorack.Modules
import           Hakyll hiding (tagsField)
import           Lucid hiding (for_)
import           System.FilePath (takeBaseName)

main :: IO ()
main = hakyllWith config $ do
  let pages = ["about.markdown", "contact.markdown", "Eurorack/links.markdown"]
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match (fromList pages) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

  tags <- buildTags ("posts/**" .||. "Eurorack/jams/*") (fromCapture "tags/*.html")
  categories <- buildCategories "posts/**" (fromCapture "categories/*.html")
  manufacturers <- buildTagsWith (getTags' "manufacturers") "Eurorack/Modules/*" (fromCapture "Eurorack/Manufacturers/*.html")
  provides <- buildTagsWith (getTags' "provides") "Eurorack/Modules/*" (fromCapture "Eurorack/Features/*.html")
  let moduleCtx = moduleField "title" (pure . show . fullName)
               <> moduleNameField "name"
               <> moduleField "description" (maybe empty (pure . show) . description)
               <> moduleField "synopsis" (maybe empty (pure . show) . synopsis)
               <> moduleCurrentField "current"
               <> moduleLengthField "width" width
               <> moduleLengthField "height" height
               <> moduleFrontPanelField "frontPanel"
               <> providesField "provides" provides
               <> manufacturersField "manufacturers" manufacturers
               <> defaultContext

  match "Eurorack/jams/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/jam.html" (tagsField "tags" tags <> postCtx)
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls

  match "posts/**" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/post.html" (tagsField "tags" tags <> postCtx)
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll ("posts/**" .||. "Eurorack/jams/*")
      let indexCtx =
              listField "posts" postCtx (pure posts) <>
              constField "title" "Home"                <>
              defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

  match "Eurorack/*.yaml" $ do
    route $ setExtension "html"
    compile $ rackCompiler
          >>= loadAndApplyTemplate "templates/rack.html" postCtx
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls

  for_ [minBound .. maxBound] $ \mod ->
    create ([fromFilePath $ unpack $ "Eurorack/Modules/" <> identifier mod <> ".markdown"]) $ do
      route $ setExtension "html"
      compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/module.html" moduleCtx
            >>= loadAndApplyTemplate "templates/default.html" moduleCtx
            >>= relativizeUrls

  match "Eurorack/Modules.html" $ do
    route idRoute
    compile $ do
      modules <- loadAll ("Eurorack/Modules/*")
      let indexCtx = listField "modules" moduleCtx (pure modules)
                  <> field "manufacturers" (const $ renderTagList manufacturers)
                  <> tagCloudField "features" 70 150 provides
                  <> defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "jams.html" $ do
    route idRoute
    compile $ do
      jams <- recentFirst =<< loadAll "Eurorack/jams/*"
      let ctx = listField "jams" postCtx (pure jams) <> defaultContext
      getResourceBody
        >>= applyAsTemplate ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  tagsRules categories $ \category pattern -> do
    let title = "Posts in category " <> category
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx = constField "title" title
             <> listField "posts" postCtx (pure posts)
             <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  tagsRules tags $ \tag pattern -> do
    let title = "Posts with tag " <> tag
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx = constField "title" title
             <> listField "posts" postCtx (pure posts)
             <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  tagsRules manufacturers $ \manufacturer pattern -> do
    let title = "Modules by " <> manufacturer
    route idRoute
    compile $ do
      modules <- loadAll pattern
      let ctx = constField "title" title
             <> listField "modules" moduleCtx (pure modules)
             <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/manufacturer.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  tagsRules provides $ \feature pattern -> do
    let title = feature <> "s"
    route idRoute
    compile $ do
      modules <- loadAll pattern
      let ctx = constField "title" title
             <> listField "modules" moduleCtx (pure modules)
             <> defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/feature.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = teaserField "teaser" "content"
       <> dateField "date" "%B %e, %Y"
       <> defaultContext

rackCompiler :: Compiler (Item String)
rackCompiler = getResourceLBS >>= traverse go where
  go yaml = case decodeEither $ toStrict $ yaml :: Either String System of
    Left e -> error e
    Right v -> if all verify v
               then pure . show $ systemHtml v
               else error "illegal rack"

verify (Case "A100LMB" rows) = length rows == 2
verify (Case "A100LMS9" rows) = length rows == 3

config = defaultConfiguration {
  deployCommand = "rsync -avcz _site/ mlang@blind.guru:blind.guru/modular/"
}

maybeModule :: Item a -> Maybe Module
maybeModule item = let ident = pack . takeBaseName . toFilePath $ itemIdentifier item
                   in lookup ident $
                      [(identifier mod, mod) | mod <- [minBound .. maxBound]]

moduleField :: String -> (Module -> Compiler String) -> Context a
moduleField key f = field key $ \item -> maybe
  (error $ "moduleField: Can't find module in " <> show (itemIdentifier item)) f $
  maybeModule item

moduleNameField :: String -> Context String
moduleNameField key = moduleField key (pure . show . toHtml . name)

moduleFrontPanelField :: String -> Context String
moduleFrontPanelField key = moduleField key (pure . show . frontPanelHtml)

moduleFunctionField :: String -> (Module -> [String] -> Compiler String) -> Context String
moduleFunctionField key f = functionField key $ \args item ->
  maybe (error $ "moduleFunctionField: Can't find module in " <> show (itemIdentifier item))
        (flip f args) $ maybeModule item

moduleLengthField :: String -> (Module -> Length) -> Context String
moduleLengthField key f = moduleFunctionField key go where
  go mod ["HP"] = pure . show . round $ f mod # HorizontalPitch
  go mod ["U"] = pure . show . round $ f mod # RackUnit
  go mod ["mm"] = pure . show . round $ f mod # milli Meter
  go mod [] = go mod ["HP"]
  go mod _ = error $ key <> "(): unsupported arguments"

moduleCurrentField :: String -> Context String
moduleCurrentField key = moduleFunctionField key f where
  f mod ["+12V", "mA"] = let Currents mA _ _ = currents mod in
                         pure . show . round $ mA # milli Ampere
  f mod ["-12V", "mA"] = let Currents _ mA _ = currents mod in
                         pure . show . round $ mA # milli Ampere
  f mod ["+5V", "mA"] = let Currents _ _ mA = currents mod in
                         pure . show . round $ mA # milli Ampere
  f mod [k] = f mod [k, "mA"]
  f mod _ = error $ key <> "(): Unsupported arguments"

-- Reimplement tagsFieldWith to fail if zero tags were found in metadata

simpleRenderLink :: String -> Maybe FilePath -> Maybe (Html ())
simpleRenderLink _   Nothing         = Nothing
simpleRenderLink tag (Just filePath) =
  Just $ a_ [href_ $ pack $ toUrl filePath] $ toHtml tag

tagsFieldWith' :: (Identifier -> Compiler [String])
              -> (String -> Maybe FilePath -> Maybe (Html ()))
              -> ([Html ()] -> Html ())
              -> String
              -> Tags
              -> Context a
tagsFieldWith' getTags'' renderLink cat key tags = field key $ \item -> do
  tags' <- getTags'' $ itemIdentifier item
  links <- for tags' $ \tag -> renderLink tag <$> getRoute (tagsMakeId tags tag)

  if (length links == 0)
  then empty
  else pure . show . cat . catMaybes $ links

tagsField :: String -> Tags -> Context a
tagsField = tagsFieldWith' (getTags' "tags") simpleRenderLink (mconcat . intersperse ", ")

manufacturersField :: String -> Tags -> Context a
manufacturersField = tagsFieldWith' (getTags' "manufacturers") simpleRenderLink (mconcat . intersperse ", ")

providesField :: String -> Tags -> Context a
providesField = tagsFieldWith' (getTags' "provides") simpleRenderLink (mconcat . intersperse ", ")

getTags' :: MonadMetadata m => String -> Identifier -> m [String]
getTags' key = fmap f . getMetadata where
  f metadata = fromMaybe [] $
               (lookupStringList key metadata) `mplus`
               (map trim . splitAll "," <$> lookupString key metadata)


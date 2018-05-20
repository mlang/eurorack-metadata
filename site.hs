--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative (Alternative(..))
import           Data.ByteString.Lazy (toStrict)
import           Data.Foldable (for_)
import           Data.Maybe (catMaybes)
import           Data.Metrology ((#))
import           Data.Metrology.SI (Length)
import           Data.Semigroup ((<>))
import           Data.Text (unpack)
import           Data.Units.SI (Ampere(..), Meter(..))
import           Data.Units.SI.Prefixes (centi, milli)
import           Data.Yaml
import           Eurorack.Modules
import           Hakyll hiding (titleField)
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

  match "Eurorack/jams/*" $ do
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
      posts <- recentFirst =<< loadAll ("posts/**" .||. "Eurorack/jams/*")
      let indexCtx =
              listField "posts" postCtx (return posts) <>
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
      compile $ pandocCompiler >>=
                loadAndApplyTemplate "templates/module.html" moduleCtx >>=
                loadAndApplyTemplate "templates/default.html" moduleCtx >>=
                relativizeUrls

  match "Eurorack/Modules.html" $ do
    route idRoute
    compile $ do
      modules <- loadAll ("Eurorack/Modules/*")
      let indexCtx =
              listField "modules" moduleCtx (return modules) <>
              defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

  match "jams.html" $ do
    route idRoute
    compile $ do
      jams <- recentFirst =<< loadAll "Eurorack/jams/*"
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

moduleCtx :: Context String
moduleCtx = moduleTitleField "title"
         <> moduleSynopsisField "synopsis"
         <> moduleCurrentField "current"
         <> moduleLengthField "width" width
         <> moduleLengthField "height" height
         <> moduleFrontPanelField "frontPanel"
         <> defaultContext

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

maybeModule :: Item a -> Maybe Module
maybeModule item = let ident = takeBaseName $ toFilePath $ itemIdentifier item in
                   lookup ident $
                   map (\mod -> (unpack $ identifier mod, mod)) [minBound .. maxBound]

moduleFieldWith :: String -> (Module -> Compiler String) -> Context a
moduleFieldWith key f = field key $ \item ->
  maybe (fail $ "moduleFieldWith: Can't find module in " <> show (itemIdentifier item))
        f $ maybeModule item

moduleTitleField :: String -> Context String
moduleTitleField key = moduleFieldWith key (pure . show . fullName)

moduleNameField :: String -> Context String
moduleNameField key = moduleFieldWith key (pure . show . toHtml . name)

moduleSynopsisField :: String -> Context String
moduleSynopsisField key = moduleFieldWith key $ \mod ->
  maybe empty (pure . show) (synopsis mod)

moduleFrontPanelField :: String -> Context String
moduleFrontPanelField key = moduleFieldWith key (pure . show . frontPanelHtml)

moduleFunctionField :: String -> (Module -> [String] -> Compiler String) -> Context String
moduleFunctionField key f = functionField key go where
  go args item = maybe (error $ "moduleFunctionField: Can't find module in " <> show (itemIdentifier item))
                       (flip f args) $ maybeModule item

moduleLengthField :: String -> (Module -> Length) -> Context String
moduleLengthField key f = moduleFunctionField key go where
  go mod ["HP"] = pure . show . round $ f mod # HorizontalPitch
  go mod ["U"] = pure . show . round $ f mod # RackUnit
  go mod ["mm"] = pure . show . round $ f mod # milli Meter
  go mod [] = go mod ["HP"]
  go mod _ = error $ "width(): Unsupported arguments"

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

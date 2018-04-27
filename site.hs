--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.ByteString.Lazy (toStrict)
import           Data.Semigroup ((<>))
import           Data.Text (unpack)
import           Data.Traversable (for)
import           Data.Yaml
import           Eurorack.Synthesizers
import           Hakyll
import           Lucid


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "images/*" $ do
    route   idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  match (fromList ["about.rst", "contact.markdown"]) $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/default.html" defaultContext
          >>= relativizeUrls

  match "eurorack/jams/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/jam.html" postCtx
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ pandocCompiler
          >>= saveSnapshot "content"
          >>= loadAndApplyTemplate "templates/post.html"    postCtx
          >>= loadAndApplyTemplate "templates/default.html" postCtx
          >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll ("posts/*" .||. "eurorack/jams/*")
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
          
  create ["synth.html"] $ do
    route idRoute
    compile $ do
        makeItem "" >>= applyTemplate synthTemplate postCtx
                    >>= loadAndApplyTemplate "templates/default.html" postCtx
                    >>= relativizeUrls

  for [minBound .. maxBound] $ \mod ->
    create [fromFilePath $ unpack $ "eurorack/modules/" <> identifier mod <> ".html"] $ do
      route idRoute
      compile $ do
        makeItem (show (moduleHtml mod)) >>=
          loadAndApplyTemplate "templates/default.html" postCtx

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
  go yaml = case decodeEither $ toStrict $ yaml :: Either String [Case1] of
    Left e -> fail $ show e
    Right v -> if all verify v then pure $ show v else fail "illegal rack"

verify (Case1 "A100LMB" rows) = length rows == 2
verify (Case1 "A100LMS9" rows) = length rows == 3

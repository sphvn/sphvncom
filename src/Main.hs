{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.HashMap.Strict as H
import qualified Data.ByteString.Char8 as C
import           Control.Applicative ((<|>))
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server (quickHttpServe)
import           Snap.Util.GZip (withCompression)

main :: IO ()
main = quickHttpServe $ withCompression site

site :: Snap ()
site = routes <|> handles <|> staticContent <|> notFound
    where
      routes        = page "blog" <|> page "purescript-jquery-setup-example" <|>
                      page "drag-and-drop-with-baconjs" <|> page "googlyfish" <|>
                      page "googlyapps"
      page x        = route [(C.pack x, serveFile $ "static/" ++ x ++ ".html")]
      handles       = route[("chrees", writeBS "thanks :)")]
      staticContent = serveDirectory' "static"
      notFound      = serveFile "static/404.html"

serveDirectory' :: MonadSnap m => FilePath -> m ()
serveDirectory' = serveDirectoryWith config
    where
        config = fancyDirectoryConfig {
              indexFiles = ["index.html"]
            , mimeTypes  = newMimeTypes `H.union` defaultMimeTypes
            }

        newMimeTypes = H.fromList [
                 (".appcache", "text/cache-manifest")
               , (".ttf",      "font/truetype")
               , (".otf",      "font/opentype")
               , (".eot",      "application/vnd.ms-fontobject")
               , (".woff",     "application/font-woff")
               ]

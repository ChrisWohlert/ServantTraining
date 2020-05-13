{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import Data.Aeson.Types
import Data.List
import Data.Maybe
import GHC.Generics
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Text.Blaze
import Text.Blaze.Html
import Text.Blaze.Html5 hiding (map, main)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
import WaiAppStatic.Storage.Filesystem


type Api = Get '[Html] (Layout Welcome)
      :<|> "books" :> Get '[JSON, Html] (Layout [Book])
      :<|> "dist" :> Raw

data Welcome = Welcome

data Book = Book { bookISBN :: String
                 , bookTitle :: String
                 } deriving (Generic, Show)

newtype Layout a = Layout a 

server :: Server Api
server = welcome 
    :<|> getBooks
    :<|> serveDirectoryWebApp "dist"

welcome = return $ Layout Welcome

getBooks = return $ Layout [Book "isbn" "title", Book "isbn2" "title2"]

instance Accept Html where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToMarkup a => MimeRender Html a where
    mimeRender _ = renderHtml . toHtml

instance ToMarkup a => ToMarkup (Layout a) where
    toMarkup (Layout x) = docTypeHtml $ do
        htmlHead $ do
            htmlTitle "Heyo"
        body $ do
            h1 "Layout"
            toHtml x
            p "help"
            script ! type_ "text/javascript" ! src "/dist/js/common.js" $ ""
            script ! type_ "text/javascript" ! src "/dist/js/shared.js" $ ""

instance ToMarkup Welcome where
    toMarkup _ = h1 "Welcome"

instance ToMarkup Book where
    toMarkup (Book isbn title) = 
        d $ do
            p (toHtml isbn)
            p (toHtml title)

instance ToJSON Book

instance ToJSON a => ToJSON (Layout a) where
    toJSON (Layout x) = toJSON x

instance ToMarkup [Book] where
    toMarkup xs = mconcat $ map toHtml xs


d = Text.Blaze.Html5.div
htmlHead = Text.Blaze.Html5.head
htmlTitle = Text.Blaze.Html5.title

proxy :: Proxy Api
proxy = Proxy

app = serve proxy server

main :: IO ()
main = run 3000 app
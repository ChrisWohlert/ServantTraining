{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

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
import Text.Blaze.Html5 hiding (map, main, html)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Utf8
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
import WaiAppStatic.Storage.Filesystem
import Control.Monad (forever)
import Control.Monad.Reader
import Control.Monad.Catch (try)
import Control.Monad.Except
import Control.Monad.Trans (liftIO)
import Control.Concurrent (forkIO)
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()
import System.Environment (getArgs)


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

instance HtmlComponent a => ToMarkup (Layout a) where
    toMarkup = html

instance HtmlComponent a => HtmlComponent (Layout a) where
    html l@(Layout x) = docTypeHtml $ do
        htmlHead $ do
            htmlTitle "Heyo"
            link ! rel "stylesheet" ! href "dist/css/shared.css"
        body $ do
            h1 "Layout"
            html x
            p "help"
            insertScripts
                where
                    insertScripts = mconcat $ map toScript (scripts l ++ scripts x)
                    toScript (Script src') = script ! type_ "text/javascript" ! src src' $ ""
    scripts l = [Script "/dist/js/common.js", Script "/dist/js/shared.js"]

instance HtmlComponent Welcome where
    html Welcome = h1 "Welcome"

instance HtmlComponent Book where
    html (Book isbn title) = 
        d ! class_ "book" $ do
            p (toHtml isbn)
            p (toHtml title)

instance ToJSON Book

instance ToJSON a => ToJSON (Layout a) where
    toJSON (Layout x) = toJSON x

instance HtmlComponent [Book] where
    html xs = mconcat $ map html xs

class HtmlComponent a where
    html :: a -> Html
    scripts :: a -> [Script]
    scripts x = []

data Script = Script AttributeValue

d = Text.Blaze.Html5.div
htmlHead = Text.Blaze.Html5.head
htmlTitle = Text.Blaze.Html5.title

proxy :: Proxy Api
proxy = Proxy

app = serve proxy server

main :: IO ()
main = run 3000 app



-- COOKIE Sessions

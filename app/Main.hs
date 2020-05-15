{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Network.Wai.Handler.WarpTLS
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
import Debug.Trace

type Api auths = Unprotected
            :<|> "books" :> Get '[JSON, Html] (Layout [Book])
            :<|> (Servant.Auth.Server.Auth auths User :> Protected)
            :<|> "dist" :> Raw

type Protected
   = "name" :> Get '[JSON, Html] String
 :<|> "email" :> Get '[JSON, Html] String

type Unprotected =
 "login" :> Capture "name" String :> Capture "email" String :> Get '[JSON, Html] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

data Welcome = Welcome

data Book = Book { bookISBN :: String
                 , bookTitle :: String
                 } deriving (Generic, Show)

newtype Layout a = Layout a 

server :: CookieSettings -> JWTSettings -> Server (Api auths)
server cs jwts = unprotected cs jwts
            :<|> getBooks
            :<|> protected
            :<|> serveDirectoryWebApp "dist"

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

proxy :: Proxy (Api '[Cookie])
proxy = Proxy

main :: IO ()
main = do
  -- We *also* need a key to sign the cookies
  myKey <- generateKey
  -- Adding some configurations. 'Cookie' requires, in addition to
  -- CookieSettings, JWTSettings (for signing), so everything is just as before
  let jwtCfg = defaultJWTSettings myKey
      cfg = defaultCookieSettings :. jwtCfg :. EmptyContext
      --- Here is the actual change
      api = Proxy :: Proxy (Api '[Cookie])
      tlsOpts = tlsSettings "C:/Users/CWO/haskell.crt" "C:/Users/CWO/haskell.key"
      warpOpts = setPort 3000 defaultSettings
  runTLS tlsOpts warpOpts $ serveWithContext api cfg (server defaultCookieSettings jwtCfg)



-- COOKIE Sessions

data Auth (auths :: [*]) val

data AuthResult val
  = BadPassword
  | NoSuchUser
  | Authenticated val
  | Indefinite

data User = User { username :: String, userEmail :: String }
   deriving (Eq, Show, Read, Generic)

instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User

data Login = Login { loginUsername :: String, loginPassword :: String }
   deriving (Eq, Show, Read, Generic)

instance ToJSON Login
instance FromJSON Login

protected :: Servant.Auth.Server.AuthResult User -> Server Protected
-- If we get an "Authenticated v", we can trust the information in v, since
-- it was signed by a key we trust.
protected (Servant.Auth.Server.Authenticated user) = return (username user) :<|> return (userEmail user)
-- Otherwise, we return a 401.
protected _ = trace ("Dafug?") throwAll err401

unprotected :: CookieSettings -> JWTSettings -> String -> String -> Server (Get '[JSON, Html] (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent))
unprotected cs jwts u e = checkCreds cs jwts (trace (u ++ " - " ++ e) (Login u e))

checkCreds cookieSettings jwtSettings (Login "name" "email") = do
   -- Usually you would ask a database for the user info. This is just a
   -- regular servant handler, so you can follow your normal database access
   -- patterns (including using 'enter').
   let usr = trace ("authing") $ User "name" "email"
   mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings usr
   case mApplyCookies of
     Nothing           -> throwError err401
     Just applyCookies -> return $ applyCookies NoContent
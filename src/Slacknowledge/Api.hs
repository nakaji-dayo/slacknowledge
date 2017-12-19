{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -fforce-recomp #-}

module Slacknowledge.Api
    ( startApp
    , app
    , API'
    ) where

import Data.Maybe
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy      as BS
import           Data.Text                 (Text, append, unpack)
import qualified Data.Text                 as T
import           Network.Wai
import           Network.Wai.Handler.Warp
import qualified Network.Wreq              as W
import           Servant
import           Servant.HTML.Blaze
import           Servant.Utils.StaticFiles
import qualified Slacknowledge.ES          as ES
import           Slacknowledge.Type
import           Slacknowledge.Db
import           Slacknowledge.Relation
import           Text.Blaze                (Markup)
import           Text.Blaze.Renderer.Utf8  (renderMarkup)
import           Text.Heterocephalus
import           Text.Regex.PCRE
import           Web.Slack                 (formatSlackTimeStamp)
import Data.SimpleSearchQuery

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = Get '[HTML] Markup
  :<|> "search" :> QueryParam "q" Text :> Get '[HTML] Markup
  :<|> "threads" :> Capture "id" Text :> Get '[HTML] Markup
  :<|> "slack-auth-redirect" :> QueryParam "code" Text :> Get '[HTML] Markup
  :<|> "howto" :> Get '[HTML] Markup
  :<|> "static" :> Raw
  :<|> "threads" :> Capture "id" Text :> "tags" :> ReqBody '[JSON] PostTagRequest :> Post '[JSON] Text

type API' = "threads" :> Capture "id" Text :> "tags" :> ReqBody '[JSON] PostTagRequest :> Post '[JSON] Text

startApp :: IO ()
startApp = do
  putStrLn "listen 8080"
  run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = (searchR Nothing)
         :<|> searchR
         :<|> detailHtml
         :<|> slackAuthRedirect
         :<|> howtoR
         :<|> serveDirectoryWebApp "static"
         :<|> postTagR



searchR :: Maybe Text -> Handler Markup
searchR mq = do
  m_param <- case parseSearchQuery <$> mq of
    Just (Left _) -> throwError err400
    Just (Right r) -> return $ Just $ ES.fromQueryMap r
    Nothing -> return Nothing
  ethreads <- liftIO $ runExceptT $ ES.searchThread m_param
  return $ case ethreads of
    Right threads -> $(compileTextFile "templates/dist/index.html")
    Left e        ->  $(compileTextFile "templates/dist/500.html")

detailHtml :: Text -> Handler Markup
detailHtml id = do
  let mq = Nothing
  ethread <- liftIO . runExceptT $ ES.getThread id
  liftIO $ print ethread
  case ethread of
    Right thread -> return $(compileTextFile "templates/dist/detail.html")
    Left e -> do
      liftIO $ print e
      return $(compileTextFile "templates/dist/500.html")


postTagR :: Text -> PostTagRequest -> Handler Text
postTagR id req = do
  let mq = Nothing
  ethread <- liftIO . runExceptT $ ES.getThread id
  liftIO $ print ethread
  let name = postTagRequestName req
  when ((T.length name) > 20) $ throwError err400
  when (not $ (unpack name) =~ ("^[a-z0-9\\-]+$":: String)) $ throwError err400
  case ethread of
    Right thread -> do
      when (elem name (thread ^. tags)) $ throwError err409
      let thread' = thread & tags .~ (name:(thread ^. tags))
      liftIO $ ES.indexThread thread'
      return "ok"
    Left e -> do
      liftIO $ print e
      throwError err404

slackAuthRedirect :: Maybe Text -> Handler Markup
slackAuthRedirect (Just code) = do
  let mq = Nothing
  res <- liftIO $ getAccessToken code
  case res of
    Right (OAuthResponse accessToken teamId teamName (OAuthResponseBot botAccessToken)) -> do
      liftIO $ runInsert' $ insertTeam (unpack teamId) (unpack teamName) (unpack accessToken) (unpack botAccessToken)
      return $(compileTextFile "templates/dist/slack_auth_redirect.html")
    Left e -> do
      liftIO $ putStrLn e
      return $(compileTextFile "templates/dist/500.html")
slackAuthRedirect _ = do
  let mq = Nothing
  return $(compileTextFile "templates/dist/500.html")

howtoR :: Handler Markup
howtoR = do
  let mq = Nothing
  return $(compileTextFile "templates/dist/howto.html")

getAccessToken code = do
  res <- liftIO $ W.get $ unpack ("https://slack.com/api/oauth.access?client_id=4214417927.281859902129&client_secret=95f18a982b24ac88f1ab542dd6e5a6b1&code=" `append` code)
  return (eitherDecode (res ^. W.responseBody) :: Either String OAuthResponse)

roundText :: Int -> Text -> Text
roundText n x
  | (T.length x) > n = T.take n x `T.append` "..."
  | otherwise = x

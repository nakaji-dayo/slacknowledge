{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}


module Slacknowledge.Type where

import           Control.Lens.TH
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy as BL
import           Data.Char
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.Text.IO         as T
import           GHC.Generics
import           Web.Slack

toSnake :: String -> String
toSnake (a:b:c)
  | isAlpha a && (isUpper b || isDigit b) = toLower a : '_' : toSnake (toLower b : c)
  | otherwise = toLower a : toSnake (b:c)
toSnake [x] = [toLower x]
toSnake [] = []

encode' :: ToJSON a => a -> Text
encode' = T.decodeUtf8 . BL.toStrict . encode

fromJSON' :: (FromJSON a, MonadError T.Text m) => Value -> m a
fromJSON' x = case fromJSON x of
    Error e   -> throwError (T.pack e)
    Success r -> return r

data History = History
  { _historyMessages :: [HistoryMessage]
  } deriving (Generic, Show)

data HistoryMessage = HistoryMessage
  { _historyMessageText     :: Text
  , _historyMessageUser     :: UserId
 --  , _historyMessageType :: Text
  , _historyMessageTs       :: SlackTimeStamp
  , _historyMessageThreadTs :: Maybe SlackTimeStamp
  } deriving (Generic, Show)

data UserInfo = UserInfo
  { _userInfoUser :: User
  } deriving (Generic, Show)

instance FromJSON History where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = toSnake . drop 8}
instance FromJSON HistoryMessage where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = toSnake . drop 15}
instance FromJSON UserInfo where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = toSnake . drop 9}

makeFields ''History
makeFields ''HistoryMessage
makeFields ''UserInfo

data Thread = Thread
  { _threadText      :: Text
  , _threadUserId    :: Text
  , _threadUserName  :: Text
  , _threadUserImage :: Text
  , _threadTs        :: SlackTimeStamp
  , _threadTeamId    :: Text
  , _threadTeamName  :: Text
  , _threadChannelId :: Text
  , _threadReplies   :: [ThreadReply]
  , _threadTags      :: [Text]
  } deriving (Generic, Show)

data ThreadReply = ThreadReply
  { _threadReplyText      :: Text
  , _threadReplyUserId    :: Text
  , _threadReplyUserName  :: Text
  , _threadReplyUserImage :: Text
  , _threadReplyTs        :: SlackTimeStamp
  } deriving (Generic, Show)

instance ToJSON ThreadReply where
  toEncoding = genericToEncoding $ defaultOptions {fieldLabelModifier = toSnake . drop 12}
instance FromJSON ThreadReply where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = toSnake . drop 12}
makeFields ''ThreadReply

instance ToJSON Thread where
  toEncoding = genericToEncoding $ defaultOptions {fieldLabelModifier = toSnake . drop 7}
instance FromJSON Thread where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = toSnake . drop 7}
makeFields ''Thread

data OAuthResponse = OAuthResponse
  { _oAuthResponseAccessToken :: Text
  , _oAuthResponseTeamId      :: Text
  , _oAuthResponseTeamName    :: Text
  , _oAuthResponseBot         :: OAuthResponseBot
  } deriving (Generic, Show)
data OAuthResponseBot = OAuthResponseBot
  { _oAuthResponseBotBotAccessToken :: Text
  } deriving (Generic, Show)

instance FromJSON OAuthResponse where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = toSnake . drop 14}
makeFields ''OAuthResponse
instance FromJSON OAuthResponseBot where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = toSnake . drop 17}
makeFields ''OAuthResponseBot

data PostTagRequest = PostTagRequest
  { postTagRequestName :: Text
  } deriving (Generic, Show)

postTagRequestModifier = toSnake . drop 14

instance FromJSON PostTagRequest where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = postTagRequestModifier}
instance ToJSON PostTagRequest where
  toEncoding = genericToEncoding $ defaultOptions {fieldLabelModifier = postTagRequestModifier}
makeLenses ''PostTagRequest

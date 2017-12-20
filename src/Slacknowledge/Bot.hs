{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}


module Slacknowledge.Bot where

import Slacknowledge.Type
import           Control.Monad
import Control.Monad.Except
import           Data.Maybe
import           System.Environment (lookupEnv)
import           Web.Slack
import           Web.Slack.WebAPI
import qualified Network.Wreq as W
import Control.Lens hiding ((??))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as BL
import GHC.Generics
import Slacknowledge.ES
import Data.Aeson.Encode.Pretty

run :: IO ()
run = do
  conf <- mkBotConfig
  migration
  withSlackHandle conf (knowledgeBot "T4LFB6C4C" "haskell-jp" ["haskell"])

mkBotConfig :: IO SlackConfig
mkBotConfig = do
  x <- lookupEnv "SLACK_API_TOKEN"
  let apiToken = "xoxb-289650422997-mwYSuE9GyT011TiJ1o07icvq"
  return SlackConfig{ _slackApiToken = apiToken }

mkApiConfig :: IO SlackConfig
mkApiConfig = do
  let apiToken = "xoxp-156521216148-179942123621-288990224225-f15e4e1a458f91376e6f79f69305a11a"
  return SlackConfig{ _slackApiToken = apiToken }

knowledgeBot :: Text -> Text -> [Text] -> SlackHandle -> IO ()
knowledgeBot teamId teamName defaultTags h = forever $ do
  getNextEvent h >>= \case
    (ReactionAdded _ "memo" _ (EmbeddedMessageItem cid msg_ts) _) -> do
      putStrLn "ReactionAdded"
      indexed <- indexThreadFromMsgTs teamId teamName defaultTags cid msg_ts
      case indexed of
        Right idxd -> void . runExceptT $ addReaction h cid "ok_hand" (idxd ^. ts)
        Left e -> T.putStrLn e
    HiddenMessage cid _ ts (Just (SMessageReplied tts)) -> do
      putStrLn $ "HiddenMessage"
      ethread <- runExceptT $ getThread (formatSlackTimeStamp tts)
      case ethread of
        Right _ -> void $ indexThreadFromMsgTs teamId teamName defaultTags cid tts
        Left _ -> putStrLn "replied to unrelated message"
    ChannelJoined (Channel { _channelId = cid }) -> do
      sendMessage h cid "Hello. I am slacknowledge bot.\nWhen a thread is added a :memo: (memo) reaction, I will save it.\nEveryone can view the saved thread at https://slacknowledge.pig-brewing.com ."
    event@(Message cid _ msg _ _ _) -> do
      let myId = ((getSession h) ^. slackSelf . selfUserId . getId)
      case msg of
        (T.stripPrefix ("<@" `T.append` myId `T.append` ">") -> Just mbody) -> do
          when ("hey" `T.isInfixOf` mbody) $ sendMessage h cid "hey!"
        _ -> putStrLn $ "unhandled event: " ++ (show event)
    event -> do
      putStrLn $ "unhandled event: " ++ (show event)
      return ()

indexThreadFromMsgTs ::
  Text -> Text -> [Text] -> ChannelId -> SlackTimeStamp -> IO (Either Text Thread)
indexThreadFromMsgTs teamId teamName defaultTags cid msg_ts = do
  conf <- mkApiConfig
  ethread <- runExceptT $ do
    root_msg <- getRootMessage conf cid msg_ts
    root_thread <- buildThread teamId teamName defaultTags conf root_msg
    reps <- getReplies conf cid (root_msg ^. ts)
    rep_threads <- mapM (buildThreadReply conf) (reps ^. messages)
    return $ root_thread & replies .~ rep_threads
  case ethread of
    Right thread -> indexThread thread >> return (Right thread)
    Left e -> T.putStrLn e >> return (Left "failed to make thread")


getRootMessage conf cid msg_ts = do
  hist <- getMessage conf cid msg_ts
  case hist of
    History (msg:_) -> do
      case (msg ^. threadTs) of
        Just tts -> if (tts == (msg ^. ts)) then return msg else getRootMessage conf cid (tts)
        _ -> return msg

buildThread
  :: (MonadError Text m, MonadIO m)
  => Text -> Text -> [Text]
  -> SlackConfig
  -> HistoryMessage
  -> m Thread
buildThread teamId teamName defaultTags conf msg = do
  u <- getUser conf (msg ^. user)
  return $ Thread (msg ^. text) (msg ^. user . getId)
    (_userName $ u ^. user) (u ^. user . userProfile . profileImage192) (msg ^. ts) teamId teamName [] defaultTags

buildThreadReply
  :: (MonadError Text m, MonadIO m)
  => SlackConfig
  -> HistoryMessage
  -> m ThreadReply
buildThreadReply conf msg = do
  u <- getUser conf (msg ^. user)
  return $ ThreadReply (msg ^. text) (msg ^. user . getId)
    (_userName $ u ^. user) (u ^. user . userProfile . profileImage192) (msg ^. ts)

getMessage
    :: (MonadError T.Text m, MonadIO m)
    => SlackConfig
    -> ChannelId
    -> SlackTimeStamp
    -> m History
getMessage conf (Id cid) msg_ts = do
  res <- makeSlackCall conf "channels.history" $
    (W.param "channel"     .~ [cid]) .
    (W.param "latest"      .~ [encode' msg_ts]) .
    (W.param "inclusive"   .~ ["true"]) .
    (W.param "count"       .~ ["1"])
  x <- fromJSON' res
  return x

getReplies
    :: (MonadError T.Text m, MonadIO m)
    => SlackConfig
    -> ChannelId
    -> SlackTimeStamp
    -> m History
getReplies conf (Id cid) msg_ts = do
  res <- makeSlackCall conf "channels.replies" $
    (W.param "channel"     .~ [cid]) .
    (W.param "thread_ts"      .~ [encode' msg_ts])
  x <- fromJSON' res
  return x

getUser
    :: (MonadError T.Text m, MonadIO m)
    => SlackConfig
    -> UserId
    -> m UserInfo
getUser conf (Id uid) = do
  res <- makeSlackCall conf "users.info" $
    (W.param "user"     .~ [uid])
  x <- fromJSON' res
  return x

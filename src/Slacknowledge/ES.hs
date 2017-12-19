{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE OverloadedStrings #-}

module Slacknowledge.ES where

import Control.Monad.Except
import Slacknowledge.Type
import           Data.Aeson             (Value, eitherDecode, decode)
import           Data.Aeson.QQ
import           Database.V5.Bloodhound
import           Network.HTTP.Client    (defaultManagerSettings)
import Control.Monad
import Control.Monad.IO.Class
import Control.Lens ((^.))
import           Web.Slack (formatSlackTimeStamp)
import Network.HTTP.Client (responseBody)
import Data.Maybe
import qualified Data.ByteString.Lazy as BL
import Data.Aeson.Encode.Pretty
import Data.Text (Text, pack)
import Slacknowledge.Config
import Data.Yaml.Config
import Data.SimpleSearchQuery
import qualified Data.Map as M

runBH' :: BH IO a -> IO a
runBH' x = do
  config <- loadYamlSettings ["config.yaml"] [] useEnv
  withBH defaultManagerSettings (Server $ pack $ esHost config) $ x

migration :: IO()
migration = runBH' $ do
  _ <- createIndex indexSettings index
  True <- indexExists index
  rs <-  putMapping index threadMapping ThreadMapping
  liftIO $ print rs

server = (Server "http://localhost:9200")
index = IndexName "slacknowledge"
indexSettings = IndexSettings (ShardCount 1) (ReplicaCount 0)
threadMapping = MappingName "thread"

data ThreadMapping = ThreadMapping deriving (Eq, Show)

instance ToJSON ThreadMapping where
  toJSON _ =
    [aesonQQ|
{
  "properties": {
    "ts": {
      "type": "double"
    },
    "tags": {
      "type": "string",
      "index": "not_analyzed"
    }
  }
}
|]

indexThread :: Thread -> IO ()
indexThread thread = runBH' $ do
  res <- indexDocument index threadMapping defaultIndexDocumentSettings
    thread (DocId  (formatSlackTimeStamp $ thread ^. ts))
  liftIO $ print res

data SearchParam = SearchParam
  { spKeywords :: [Text]
  , spTags :: [Text]
  }

fromQueryMap :: QueryMap -> SearchParam
fromQueryMap m = SearchParam
  { spKeywords = M.findWithDefault [] Keyword m
  , spTags = M.findWithDefault [] (Label "tag") m
  }

searchThread :: (MonadError String m, MonadIO m) => Maybe SearchParam -> m [Thread]
searchThread mparam = do
  let mk_match k = QueryMultiMatchQuery $ MultiMatchQuery
        { multiMatchQueryFields = [FieldName "text", FieldName "user_name", FieldName "tags", FieldName "replies.text"]
        , multiMatchQueryString = (QueryString k)
        , multiMatchQueryOperator = And
        , multiMatchQueryZeroTerms = ZeroTermsNone
        , multiMatchQueryTiebreaker = Nothing
        , multiMatchQueryType = Nothing
        , multiMatchQueryCutoffFrequency = Nothing
        , multiMatchQueryAnalyzer = Nothing
        , multiMatchQueryMaxExpansions = Nothing
        , multiMatchQueryLenient = Nothing
        }
  let mk_query p = QueryBoolQuery $ BoolQuery
        { boolQueryMustMatch = fmap mk_match (spKeywords p)
        , boolQueryFilter = map (\tag -> Filter $ TermQuery (Term "tags" tag) Nothing) (spTags p)
        , boolQueryMustNotMatch = []
        , boolQueryShouldMatch = []
        , boolQueryMinimumShouldMatch = Nothing
        , boolQueryBoost = Nothing
        , boolQueryDisableCoord = Nothing
        }
  res <- liftIO . runBH' $ do
    let search = Search
          { queryBody = fmap mk_query mparam
          , filterBody = Nothing
          , sortBody = Just [DefaultSortSpec (mkSort (FieldName "ts") Descending)]
          , aggBody = Nothing
          , highlight = Nothing
          , trackSortScores = False
          , from = From 0
          , size = Size 20
          , searchType = SearchTypeQueryThenFetch
          , fields = Nothing
          , source = Nothing
          }
    searchByIndex index search
  case eitherDecode (responseBody res) of
    Right res -> return $ catMaybes $ hitSource <$> (hits . searchHits $ res)
    Left e -> throwError e

getThread :: (MonadError String m, MonadIO m) => Text -> m Thread
getThread tid = do
  liftIO . print $ tid
  res <- liftIO . runBH' $ getDocument index threadMapping (DocId tid)
  case eitherDecode (responseBody res) of
    Right (EsResult {foundResult=(Just src)}) -> return (_source src)
    Left e -> throwError e
    _ -> throwError "getThread pattern match failed"

{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Slacknowledge.Config where

import           Data.Aeson
import           Data.Aeson.TH
import           GHC.Generics
import           Slacknowledge.Type
import Control.Lens

data Config = Config
  { mysqlUser :: String
  , mysqlPassword :: String
  , mysqlHost :: String
  , esHost :: String
  } deriving (Generic, Show)

instance FromJSON Config where
  parseJSON = genericParseJSON $ defaultOptions {fieldLabelModifier = toSnake}

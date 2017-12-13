{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Elm
import           GHC.Generics       (Generic)
import           Servant.API        ((:>), Capture, Get, JSON)
import           Servant.Elm        (ElmType, Proxy (Proxy), defElmImports,
                                     generateElmForAPI)
import           Slacknowledge.Api
import           Slacknowledge.Type
import Data.Text (pack, unpack, Text)

instance ElmType PostTagRequest

spec :: Spec
spec = Spec ["Generated", "Api"]
            (defElmImports
             : toElmTypeSource    (Proxy :: Proxy PostTagRequest)
             -- : toElmDecoderSource (Proxy :: Proxy PostTagRequest)
             : toElmEncoderSourceWith (Options (inText postTagRequestModifier)) (Proxy :: Proxy PostTagRequest)
             : generateElmForAPI  (Proxy :: Proxy API'))

inText :: (String -> String) -> (Text -> Text)
inText f = pack . f . unpack

main :: IO ()
main = specsToDir [spec] "elm"

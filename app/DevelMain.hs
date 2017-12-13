module Main where

import Rapid
import qualified Slacknowledge.Api as Api
import qualified Slacknowledge.Bot as Bot

main = return ()

up :: IO ()
up = rapid 0 $ \r -> do
  restart r "api" Api.startApp
  restart r "bot" Bot.run

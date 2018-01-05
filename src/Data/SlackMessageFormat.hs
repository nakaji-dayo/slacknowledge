{-# LANGUAGE OverloadedStrings #-}
module Data.SlackMessageFormat where

import           Control.Applicative
import           Data.Attoparsec.Text   as A
import           Data.Text              (Text, append, pack, unpack)
import qualified Data.Text         as T
import Debug.Trace (traceM)
import Data.Emoji

slackMessageParser :: Parser Text
slackMessageParser = do
  x <- T.concat <$> (many p)
  y <- takeText
  return $  x `T.append` y
  where
    p = do
      a <- A.takeTill (flip elem ['<', '\n', ':'])
      traceM $ "newChar: " ++ show a
      plink a <|> pbr a <|> pemoji a
    plink a = do
      char '<'
      special <- optional $ char '#' <|> char '@' <|> char '!'
      link <- A.takeTill (== '>')
      char '>'
      traceM $ "newChar: " ++ show link
      case special of
        Nothing -> return (T.concat [a, "<a href=\"", link, "\" target=\"_blank\">", link, "</a>"])
        Just s -> return $ T.cons s link
    pbr a = do
      char '\n'
      return $ T.concat [a, "<br />"]
    pemoji a = do
      char ':'
      name <- A.takeTill (== ':')
      char ':'
      return $ case unicodeByName (unpack name) of
        Just code -> T.concat [a, pack code]
        _ -> T.concat [a, ":", name, ":"]

parseSlackMessage = parseOnly slackMessageParser

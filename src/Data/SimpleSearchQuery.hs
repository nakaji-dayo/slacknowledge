{-# LANGUAGE OverloadedStrings #-}

module Data.SimpleSearchQuery where

import           Data.Attoparsec.Text       as A
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import Control.Applicative

-- data Query = Tag Text | Term Text
--   deriving (Show)

-- simple :: Parser [Query]
-- simple = many' (p_tag <|> p_term)

-- p_tag :: Parser Query
-- p_tag = Tag <$> ("tag:" *> (
--   (A.takeWhile (/= ' '))
--   ) <* char ' ')

-- p_term :: Parser Query
-- p_term = Term <$> ((A.takeWhile (/= ' ')) <* char ' ')

data Query = Query
  { keywords :: [Text]
  , tags :: [Text]
  } deriving (Show)


queryParser =
  Query
  <$> (labeled "k")
  <*> (labeled "tag")

labeled l =
  (string l *> A.takeWhile (/= ' ')) `sepBy` (char ' ')

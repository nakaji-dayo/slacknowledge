{-# LANGUAGE OverloadedStrings #-}

module Data.SimpleSearchQuery where

import           Control.Applicative
import           Data.Attoparsec.Text   as A
import           Data.Text              (Text)
import qualified Data.Text         as T
import Data.Map

type QueryMap = Map QueryKey [Text]

data QueryKey = Keyword | Label Text
  deriving (Show, Eq, Ord)

searchStringParser :: Parser QueryMap
searchStringParser = fromListWith mappend <$> f
  where
    f = g `sepBy` " "
    g = do
      k_or_v <- A.takeWhile (\x -> x /= ':' && x /= ' ')
      optional $ char ':'
      mv <- optional $ A.takeWhile1 (/= ' ')
      return $ case mv of
                 Just v -> (Label k_or_v, [v])
                 _ -> (Keyword, [k_or_v])

parseSearchQuery = parseOnly searchStringParser

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Lanthanum.Utils where

import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.List

import Database.Persist
import Servant.Common.Text
import Web.PathPieces

jsonOptions :: String -> Options
jsonOptions prefix = defaultOptions
  { fieldLabelModifier     = toSnakeCase . drop (length prefix)
  , constructorTagModifier = drop (length prefix)
  }
  where
    toSnakeCase = map toLower . intercalate "_" . splitCamelWords
    splitCamelWords = reverse . splitWordsReversed . reverse

    splitWordsReversed :: String -> [String]
    splitWordsReversed [] = []
    splitWordsReversed rs
      | null ls   = reverse us : splitWordsReversed urs
      | otherwise = case lrs of
                      []     -> [reverse ls]
                      (c:cs) -> (c : reverse ls) : splitWordsReversed cs
      where
        (ls, lrs) = span isLower rs
        (us, urs) = span isUpper rs

instance (PathPiece s) => FromText s where
  fromText = fromPathPiece

instance (PathPiece s) => ToText s where
  toText = toPathPiece

instance ToJSON a => ToJSON (Entity a) where
  toJSON Entity{..} = object
    [ "id"    .= entityKey
    , "value" .= entityVal ]


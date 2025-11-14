{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Supakell.Query where

import Data.Aeson (Value, encode)
import Data.Text (Text)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Text.Encoding    as T
import qualified Data.ByteString.Char8 as BSC

--------------------------------------------------------------------------------
-- TABLE TYPECLASS
--------------------------------------------------------------------------------

class Table t where
  tableName :: Text

--------------------------------------------------------------------------------
-- FILTERS
--------------------------------------------------------------------------------

data Filter
  = Eq Text Value
  | Gt Text Value
  | Lt Text Value
  deriving (Show, Eq)

type QueryParam = (BS.ByteString, Maybe BS.ByteString)

--------------------------------------------------------------------------------
-- JSON ENCODING FOR FILTER VALUES
--------------------------------------------------------------------------------

encodeJson :: Value -> Text
encodeJson = T.decodeUtf8 . LBS.toStrict . encode

--------------------------------------------------------------------------------
-- QUERY BUILDER
--------------------------------------------------------------------------------

data Query t = Query
  { qTable   :: Text
  , qFilters :: [Filter]
  , qLimit   :: Maybe Int
  } deriving (Show, Eq)

select :: forall t. Table t => Query t
select = Query
  { qTable   = tableName @t
  , qFilters = []
  , qLimit   = Nothing
  }

where_ :: Filter -> Query t -> Query t
where_ fl q = q { qFilters = fl : qFilters q }

limit :: Int -> Query t -> Query t
limit n q = q { qLimit = Just n }

--------------------------------------------------------------------------------
-- RENDERING TO URL PARAMS
--------------------------------------------------------------------------------

toQueryParams :: Query t -> [QueryParam]
toQueryParams q =
    concatMap renderFilter (qFilters q)
    ++ maybe [] (\n -> [("limit", Just (BSC.pack (show n)))]) (qLimit q)

pair :: Text -> Text -> QueryParam
pair k v = (T.encodeUtf8 k, Just (T.encodeUtf8 v))

renderFilter :: Filter -> [QueryParam]
renderFilter fl =
  case fl of
    Eq k v -> [ pair k ("eq." <> encodeJson v) ]
    Gt k v -> [ pair k ("gt." <> encodeJson v) ]
    Lt k v -> [ pair k ("lt." <> encodeJson v) ]

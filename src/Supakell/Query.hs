{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Supakell.Query where

import Data.Aeson (Value, encode, FromJSON, eitherDecode)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Text.Encoding    as T
import qualified Data.ByteString.Char8 as BSC
import Control.Monad.Reader (asks)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

import Supakell.Client (SupakellM, SupakellClient(..))
import Supakell.Types (Table(..))

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
  { qTable   = tableName (Nothing :: Maybe t)
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

--------------------------------------------------------------------------------
-- EXECUTION
--------------------------------------------------------------------------------

-- | Execute a query and parse the results
execute :: FromJSON a => Query t -> SupakellM (Either String [a])
execute query = do
  client <- asks id
  let url = T.unpack (baseUrl client) <> "/rest/v1/" <> T.unpack (qTable query)

  manager <- liftIO $ newManager tlsManagerSettings
  initialRequest <- liftIO $ parseRequest url

  let request = setQueryString (toQueryParams query)
              $ initialRequest
                  { requestHeaders =
                      [ ("apikey", T.encodeUtf8 (apiKey client))
                      , ("Authorization", "Bearer " <> T.encodeUtf8 (apiKey client))
                      , ("Accept", "application/json")
                      ]
                  }

  response <- liftIO $ httpLbs request manager

  let status = statusCode (responseStatus response)
  let body = responseBody response

  if status >= 200 && status < 300
    then return $ eitherDecode body
    else return $ Left $ "HTTP error " ++ show status ++ ": " ++ BSC.unpack (LBS.toStrict body)

-- | Convenience function to select all rows from a table by name
selectFrom :: FromJSON a => Text -> SupakellM (Either String [a])
selectFrom tableName' =
  execute Query
    { qTable = tableName'
    , qFilters = []
    , qLimit = Nothing
    }

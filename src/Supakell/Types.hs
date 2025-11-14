{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Supakell.Types
  ( Table(..)
  , Character(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Each Haskell type that represents a DB table implements this.
-- Example: `Character` corresponds to "characters".
class Table t where
  tableName :: proxy t -> Text

------------------------------------------
-- Example table (for testing the client)
------------------------------------------

data Character = Character
  { id   :: Int
  , name :: Text
  , age  :: Int
  }
  deriving (Show, Generic)

instance FromJSON Character
instance ToJSON Character

instance Table Character where
  tableName _ = "characters"

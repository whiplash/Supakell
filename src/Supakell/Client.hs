{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Supakell.Client
  ( SupakellClient(..)
  , SupakellM
  , createClient
  , runSupakell
  ) where

import Data.Text (Text)
import Control.Monad.Reader
import Control.Monad.IO.Class

data SupakellClient = SupakellClient
  { baseUrl :: Text
  , apiKey  :: Text
  }

newtype SupakellM a = SupakellM { unSupakellM :: ReaderT SupakellClient IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader SupakellClient)

createClient :: Text -> Text -> SupakellClient
createClient = SupakellClient

runSupakell :: SupakellClient -> SupakellM a -> IO a
runSupakell client = flip runReaderT client . unSupakellM


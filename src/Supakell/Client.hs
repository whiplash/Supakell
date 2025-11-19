{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Supakell.Client
  ( SupakellClient(..)
  , SupakellM
  , createClient
  , createClientFromEnv
  , runSupakell
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.Reader
import System.Environment (lookupEnv)
import Configuration.Dotenv (loadFile, defaultConfig)

data SupakellClient = SupakellClient
  { baseUrl :: Text
  , apiKey  :: Text
  }

newtype SupakellM a = SupakellM { unSupakellM :: ReaderT SupakellClient IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader SupakellClient)

createClient :: Text -> Text -> SupakellClient
createClient = SupakellClient

-- | Create a client by loading credentials from a .env file
-- Looks for SUPABASE_URL and SUPABASE_ANON_KEY environment variables
createClientFromEnv :: IO (Either String SupakellClient)
createClientFromEnv = do
  -- Load .env file if it exists
  _ <- loadFile defaultConfig

  -- Read environment variables
  mbUrl <- lookupEnv "SUPABASE_URL"
  mbKey <- lookupEnv "SUPABASE_ANON_KEY"

  case (mbUrl, mbKey) of
    (Just url, Just key) ->
      return $ Right $ SupakellClient (T.pack url) (T.pack key)
    (Nothing, _) ->
      return $ Left "SUPABASE_URL environment variable not set"
    (_, Nothing) ->
      return $ Left "SUPABASE_ANON_KEY environment variable not set"

runSupakell :: SupakellClient -> SupakellM a -> IO a
runSupakell client = flip runReaderT client . unSupakellM


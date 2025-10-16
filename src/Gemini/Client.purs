module Gemini.Client where

import Prelude

import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Data.Either (Either(..))

-- | Endpoint for Gemini API requests.
geminiEndpoint :: String
geminiEndpoint = "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-mini:generateContent"

-- | Fetch quotes using Gemini. Returns either an error string or the raw response body.
fetchGeneratedQuotes :: Int -> Aff (Either String String)
fetchGeneratedQuotes _ = do
  liftEffect $ log "Gemini API not implemented yet."
  pure $ Left "Gemini API not implemented"

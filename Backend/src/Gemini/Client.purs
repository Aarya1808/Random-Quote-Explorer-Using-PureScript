module Gemini.Client where

import Prelude

import Affjax.Web (post)
import Affjax.RequestBody as RequestBody
import Affjax.ResponseFormat as ResponseFormat
import Data.Argonaut.Core (jsonEmptyObject)
import Data.Argonaut.Encode.Combinators ((~>), (:=))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (length)
import Effect.Aff (Aff, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Gemini.Prompt (buildPrompt)


fetchGeneratedQuotes :: String -> Int -> Aff (Either String String)
fetchGeneratedQuotes apiKey count = do
  liftEffect $ log $ "Fetching " <> show count <> " quotes from Gemini API..."
  
  let 
    prompt = buildPrompt count
    endpoint = "https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-mini:generateContent?key=" <> apiKey
    

    requestBody = 
      ("contents" := [("parts" := [("text" := prompt)])])
      ~> jsonEmptyObject
  
  liftEffect $ log $ "Sending request to Gemini API: " <> endpoint

  
  result <- try $ post ResponseFormat.string endpoint (Just $ RequestBody.json requestBody)
  
  case result of
    Left err -> do
      liftEffect $ log $ "Gemini API connection error: " <> show err
      pure $ Left $ "API connection failed: " <> show err
    
    Right (Left _) -> do
      liftEffect $ log $ "Gemini API HTTP error"
      pure $ Left "HTTP error occurred"
    
    Right (Right response) -> do
      liftEffect $ log $ "Successfully fetched from Gemini API"
      liftEffect $ log $ "Response length: " <> show (length response.body)
      pure $ Right response.body
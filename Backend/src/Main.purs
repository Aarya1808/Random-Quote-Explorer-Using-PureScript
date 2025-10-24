module Main where

import Prelude

import Gemini.Client (fetchGeneratedQuotes)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (try)
import Effect.Aff.Class as H
import HTTPurple (Request, ResponseM)
import HTTPurple as HTTPurple
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as RD
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Array (findMap)
import Data.String (Pattern(..), split, trim)
import Node.Process (lookupEnv)
import Data.Int (fromString)


readApiKey :: Effect (Either String String)
readApiKey = do
  result <- try $ readTextFile UTF8 ".env"
  case result of
    Left err -> do
      log $ "Failed to read .env file: " <> show err
      pure $ Left "Could not read .env file"
    Right content -> do
      let
        keyValue = findMap
          (\line -> case split (Pattern "=") (trim line) of
              ["GEMINI_API_KEY", value] -> Just (trim value)
              _ -> Nothing
          )
          (split (Pattern "\n") content)
      case keyValue of
        Just key -> pure $ Right key
        Nothing -> do
          log "GEMINI_API_KEY not found in .env file"
          pure $ Left "GEMINI_API_KEY not found in .env file"


readFallbackQuotes :: Effect (Either String String)
readFallbackQuotes = do
  result <- try $ readTextFile UTF8 "quotes.json"
  case result of
    Left err -> do
      log $ "Failed to read quotes.json: " <> show err
      pure $ Left "Fallback quotes file not found"
    Right quotes -> pure $ Right quotes

route :: RouteDuplex' Unit
route = RD.root (pure unit)

corsHeaders :: HTTPurple.ResponseHeaders
corsHeaders =
  HTTPurple.header "Access-Control-Allow-Origin" "*"
  <> HTTPurple.header "Access-Control-Allow-Methods" "GET, POST, OPTIONS"
  <> HTTPurple.header "Access-Control-Allow-Headers" "Content-Type"
  <> HTTPurple.header "Content-Type" "application/json"

router :: Request Unit -> ResponseM
router { path: [], method: HTTPurple.Get } = do
  log "Received request to /"
  HTTPurple.ok' corsHeaders "Quote Explorer API - Available endpoints: /api/quotes"

router { path: ["api", "quotes"], method: HTTPurple.Get } = do
  log "Received request to /api/quotes"
  

  apiKeyResult <- liftEffect readApiKey
  
  case apiKeyResult of
    Left err -> do
      log $ "Error loading API key: " <> err <> ", falling back to static data"
      fallbackResult <- liftEffect readFallbackQuotes
      case fallbackResult of
        Left fallbackErr -> 
          HTTPurple.internalServerError' corsHeaders $ "API key error: " <> err <> " and " <> fallbackErr
        Right quotes -> 
          HTTPurple.ok' corsHeaders quotes
    
    Right key -> do
      log "GEMINI_API_KEY found, attempting Gemini API call..."
      

      geminiResult <- H.liftAff $ fetchGeneratedQuotes key 50
      case geminiResult of
        Right quotes -> do
          log "Successfully fetched from Gemini API"
          HTTPurple.ok' corsHeaders quotes
        
        Left geminiErr -> do
          log $ "Gemini API failed: " <> geminiErr
          log "Falling back to static quotes.json..."
          
          
          fallbackResult <- liftEffect readFallbackQuotes
          case fallbackResult of
            Left fallbackErr -> 
              HTTPurple.internalServerError' corsHeaders $ 
                "Gemini failed: " <> geminiErr <> " | Fallback failed: " <> fallbackErr
            Right quotes -> do
              log "Successfully loaded fallback quotes"
              HTTPurple.ok' corsHeaders quotes

router { method: HTTPurple.Options } =
  HTTPurple.ok' corsHeaders ""

router _ =
  HTTPurple.notFound' corsHeaders

main :: Effect Unit
main = do
  envPort <- lookupEnv "PORT"
  let port = case envPort >>= fromString of
        Just p -> p
        Nothing -> 8080
  log $ "Starting Quote Explorer Server on port " <> show port <> "..."
  HTTPurple.serve { port } { route, router } >>= \_ -> pure unit
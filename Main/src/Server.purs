module Server where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import HTTPurple (Request, ResponseM)
import HTTPurple as HTTPurple
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Routing.Duplex (RouteDuplex')
import Routing.Duplex as RD
import Dotenv (loadFile)
import Node.Process (lookupEnv)
import Data.String (take)
import Effect (void)

-- ROUTE
route :: RouteDuplex' Unit
route = RD.root (pure unit)

-- CORS HEADERS
corsHeaders :: HTTPurple.ResponseHeaders
corsHeaders =
  HTTPurple.header "Access-Control-Allow-Origin" "*"
    <> HTTPurple.header "Access-Control-Allow-Methods" "GET, POST, OPTIONS"
    <> HTTPurple.header "Access-Control-Allow-Headers" "Content-Type"
    <> HTTPurple.header "Content-Type" "application/json"

-- READ QUOTES JSON FILE (fallback)
readQuotesFile :: Effect String
readQuotesFile = readTextFile UTF8 "quotes.json"

-- ROUTER
router :: Request Unit -> ResponseM
router { path: [], method: HTTPurple.Get } = do
  log "Received request to /"
  HTTPurple.ok' corsHeaders "Quote Explorer API - Available endpoints: /api/quotes"

router { path: ["api", "quotes"], method: HTTPurple.Get } = do
  log "Received request to /api/quotes"
  quotesJson <- liftEffect readQuotesFile
  HTTPurple.ok' corsHeaders quotesJson

router { method: HTTPurple.Options } =
  HTTPurple.ok' corsHeaders ""

router _ =
  HTTPurple.notFound' corsHeaders

-- MAIN FUNCTION
main :: Effect Unit
main = do
  -- Load environment variables from .env
  _ <- loadFile
  maybeKey <- lookupEnv "GEMINI_API_KEY"
  case maybeKey of
    Nothing -> log "⚠️ GEMINI_API_KEY not found in environment!"
    Just key -> log $ "✅ Loaded GEMINI_API_KEY successfully (" <> take 6 key <> "...)"

  log "🚀 Starting server..."
  void $ HTTPurple.serve { port: 8080 } { route, router }
  log "✅ Server running on port 8080"

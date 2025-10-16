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


route :: RouteDuplex' Unit
route = RD.root (pure unit)

corsHeaders :: HTTPurple.ResponseHeaders
corsHeaders =
  HTTPurple.header "Access-Control-Allow-Origin" "*"
    <> HTTPurple.header "Access-Control-Allow-Methods" "GET, POST, OPTIONS"
    <> HTTPurple.header "Access-Control-Allow-Headers" "Content-Type"
    <> HTTPurple.header "Content-Type" "application/json"

readQuotesFile :: Effect String
readQuotesFile = readTextFile UTF8 "quotes.json"

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

main :: Effect Unit
main = HTTPurple.serve { port: 8080 } { route, router } >>= \_ -> pure unit

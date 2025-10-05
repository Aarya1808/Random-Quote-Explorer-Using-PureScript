module Main where

import Prelude

import Affjax (Error, Response)
import Affjax.Web (get)
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut (Json, decodeJson, printJsonDecodeError, (.:))
import Data.Array (filter, length, index, nub, sort)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (toLower, contains, Pattern(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))
import Data.Traversable (traverse)
import Data.Bifunctor (lmap)

newtype Quote = Quote
  { id :: Int
  , text :: String
  , author :: String
  , category :: String
  , year :: Maybe Int
  , source :: String
  , tags :: Array String
  }

derive instance eqQuote :: Eq Quote

newtype Category = Category
  { name :: String
  , description :: String
  , color :: String
  }

newtype Author = Author
  { name :: String
  , bio :: String
  , birth_year :: Int
  , quote_count :: Int
  }

newtype QuoteDatabase = QuoteDatabase
  { quotes :: Array Quote
  , categories :: Array Category
  , authors :: Array Author
  }

type State =
  { currentQuote :: Maybe Quote
  , allQuotes :: Array Quote
  , categories :: Array Category
  , authors :: Array Author 
  , selectedAuthor :: Maybe String
  , selectedCategory :: Maybe String
  , searchQuery :: String
  , showFilters :: Boolean
  , loading :: Boolean
  , error :: Maybe String
  }

data Action
  = GetRandomQuote
  | FilterByCategory String
  | FilterByAuthor String
  | SetSearchQuery String
  | ToggleFilters
  | ClearFilters
  | Initialize 
  | FetchQuotes
  | ReceiveQuotes (Either Error (Response Json))

-- Decode JSON responses
decodeQuote :: Json -> Either String Quote
decodeQuote json = lmap printJsonDecodeError do
  obj <- decodeJson json
  id <- obj .: "id"
  text <- obj .: "text"
  author <- obj .: "author"
  category <- obj .: "category"
  year <- obj .: "year"
  source <- obj .: "source"
  tags <- obj .: "tags"
  pure $ Quote { id, text, author, category, year, source, tags }

decodeCategory :: Json -> Either String Category
decodeCategory json = lmap printJsonDecodeError do
  obj <- decodeJson json
  name <- obj .: "name"
  description <- obj .: "description"
  color <- obj .: "color"
  pure $ Category { name, description, color }

decodeAuthor :: Json -> Either String Author
decodeAuthor json = lmap printJsonDecodeError do
  obj <- decodeJson json
  name <- obj .: "name"
  bio <- obj .: "bio"
  birth_year <- obj .: "birth_year"
  quote_count <- obj .: "quote_count"
  pure $ Author { name, bio, birth_year, quote_count }

decodeQuoteDatabase :: Json -> Either String QuoteDatabase
decodeQuoteDatabase json = do
  result <- lmap printJsonDecodeError do
    obj <- decodeJson json
    quotesJson <- obj .: "quotes"
    categoriesJson <- obj .: "categories"
    authorsJson <- obj .: "authors"
    
    quotesArray <- decodeJson quotesJson
    categoriesArray <- decodeJson categoriesJson
    authorsArray <- decodeJson authorsJson
    
    pure { quotesArray, categoriesArray, authorsArray }
  
  quotes <- traverse decodeQuote result.quotesArray
  categories <- traverse decodeCategory result.categoriesArray
  authors <- traverse decodeAuthor result.authorsArray
  
  pure $ QuoteDatabase { quotes, categories, authors }

getFilteredQuotes :: State -> Array Quote
getFilteredQuotes state =
  let
    categoryFilter = case state.selectedCategory of 
      Nothing -> identity
      Just cat -> filter (\(Quote q) -> q.category == cat)

    authorFilter = case state.selectedAuthor of
      Nothing -> identity
      Just auth -> filter (\(Quote q) -> q.author == auth)

    searchFilter = if state.searchQuery == ""
      then identity
      else filter (\(Quote q) ->
        contains (Pattern $ toLower state.searchQuery) (toLower q.text) ||
        contains (Pattern $ toLower state.searchQuery) (toLower q.author))
  in 
    state.allQuotes # categoryFilter # authorFilter # searchFilter

handleAction :: forall output slots. Action -> H.HalogenM State Action slots output Aff Unit
handleAction = case _ of
  Initialize -> do
    handleAction FetchQuotes

  FetchQuotes -> do
    H.modify_ _ { loading = true, error = Nothing }
    response <- H.liftAff $ get ResponseFormat.json "http://localhost:8080/api/quotes"
    handleAction $ ReceiveQuotes response

  ReceiveQuotes response -> case response of
    Left _ -> do
      H.modify_ _ 
        { loading = false
        , error = Just "Failed to load quotes from server. Please make sure the backend is running."
        }
    Right { body } -> do
      case decodeQuoteDatabase body of
        Left decodeErr -> do
          H.modify_ _ 
            { loading = false
            , error = Just $ "Failed to parse quotes: " <> decodeErr
            }
        Right (QuoteDatabase db) -> do
          H.modify_ _ 
            { loading = false
            , error = Nothing
            , allQuotes = db.quotes
            , categories = db.categories
            , authors = db.authors
            }
          handleAction GetRandomQuote

  GetRandomQuote -> do
    state <- H.get 
    let filteredQuotes = getFilteredQuotes state
    if length filteredQuotes == 0 
      then H.modify_ _ { currentQuote = Nothing }
      else do
        idx <- liftEffect $ randomInt 0 (length filteredQuotes - 1)
        case index filteredQuotes idx of 
          Just quote -> H.modify_ _ { currentQuote = Just quote }
          Nothing -> pure unit

  FilterByCategory category -> do
    H.modify_ _ { selectedCategory = Just category }
    handleAction GetRandomQuote

  FilterByAuthor author -> do
    H.modify_ _ { selectedAuthor = Just author }
    handleAction GetRandomQuote
    
  SetSearchQuery query -> do
    H.modify_ _ { searchQuery = query }
    
  ToggleFilters -> do
    H.modify_ \s -> s { showFilters = not s.showFilters }
    
  ClearFilters -> do
    H.modify_ _ 
      { selectedCategory = Nothing
      , selectedAuthor = Nothing
      , searchQuery = ""
      }
    handleAction GetRandomQuote
  
render :: forall slots. State -> H.ComponentHTML Action slots Aff
render state = 
  HH.div
    [ HP.class_ $ HH.ClassName "app-container"]
    [ renderHeader
    , if state.loading
        then renderLoading
        else case state.error of
          Just err -> renderError err
          Nothing -> HH.div_
            [ renderFilters state
            , renderQuoteDisplay state
            , renderStats state
            ]
    ]

renderHeader :: forall slots. H.ComponentHTML Action slots Aff
renderHeader =
  HH.div
    [ HP.class_ $ HH.ClassName "header" ]
    [ HH.h1_ [ HH.text "💬 Quote Explorer Pro" ]
    , HH.p_ [ HH.text "Discover wisdom from great minds" ]
    ]

renderLoading :: forall slots. H.ComponentHTML Action slots Aff
renderLoading =
  HH.div
    [ HP.class_ $ HH.ClassName "loading" ]
    [ HH.p_ [ HH.text "Loading quotes..." ] ]

renderError :: forall slots. String -> H.ComponentHTML Action slots Aff
renderError err =
  HH.div
    [ HP.class_ $ HH.ClassName "error" ]
    [ HH.p_ [ HH.text err ]
    , HH.button
        [ HE.onClick \_ -> FetchQuotes
        , HP.class_ $ HH.ClassName "retry-btn"
        ]
        [ HH.text "Retry" ]
    ]

renderFilters :: forall slots. State -> H.ComponentHTML Action slots Aff
renderFilters state =
  HH.div
    [ HP.class_ $ HH.ClassName "filters-section"]
    [ HH.button
        [ HP.class_ $ HH.ClassName "toggle-filters-btn"
        , HE.onClick \_ -> ToggleFilters
        ]
        [ HH.text $ if state.showFilters then "Hide Filters" else "Show Filters"]
    , if state.showFilters
        then renderFilterControls state
        else HH.div_ []
    ]

renderFilterControls :: forall slots. State -> H.ComponentHTML Action slots Aff
renderFilterControls state =
  HH.div
    [ HP.class_ $ HH.ClassName "filter-controls"]
    [ HH.input
        [ HP.class_ $ HH.ClassName "search-input" 
        , HP.type_ HP.InputText
        , HP.placeholder "Search quotes or authors..."
        , HP.value state.searchQuery
        , HE.onValueInput SetSearchQuery
        ]
    , HH.select
        [ HE.onSelectedIndexChange (\idx -> handleCategoryChange state.categories idx)
        , HP.class_ $ HH.ClassName "category-select"
        ]
        ([ HH.option [ HP.value "" ] [ HH.text "All Categories" ] ] <>
          map renderCategoryOption state.categories)
    , HH.select
        [ HE.onSelectedIndexChange (\idx -> handleAuthorChange state.allQuotes idx)
        , HP.class_ $ HH.ClassName "author-select"
        ]
        ([ HH.option [ HP.value "" ] [ HH.text "All Authors" ] ] <>
          map renderAuthorOption (getUniqueAuthors state.allQuotes))
    , HH.button
        [ HE.onClick \_ -> ClearFilters
        , HP.class_ $ HH.ClassName "clear-filters-btn"
        ]
        [ HH.text "Clear Filters" ]
    ]

renderCategoryOption :: forall slots. Category -> H.ComponentHTML Action slots Aff
renderCategoryOption (Category cat) =
  HH.option [ HP.value cat.name ] [ HH.text cat.name ]

renderAuthorOption :: forall slots. String -> H.ComponentHTML Action slots Aff
renderAuthorOption author =
  HH.option [ HP.value author ] [ HH.text author ]

handleCategoryChange :: Array Category -> Int -> Action
handleCategoryChange categories idx = 
  case idx of
    0 -> ClearFilters 
    _ -> 
      case index categories (idx - 1) of
        Just (Category cat) -> FilterByCategory cat.name
        Nothing -> ClearFilters

handleAuthorChange :: Array Quote -> Int -> Action  
handleAuthorChange quotes idx =
  case idx of
    0 -> ClearFilters 
    _ ->
      case index (getUniqueAuthors quotes) (idx - 1) of
        Just author -> FilterByAuthor author
        Nothing -> ClearFilters

getUniqueAuthors :: Array Quote -> Array String
getUniqueAuthors quotes = 
  nub $ sort $ map (\(Quote q) -> q.author) quotes

renderQuoteDisplay :: forall slots. State -> H.ComponentHTML Action slots Aff
renderQuoteDisplay state =
  HH.div 
    [ HP.class_ $ HH.ClassName "quote-section" ]
    [ case state.currentQuote of
        Nothing -> 
          HH.div 
            [ HP.class_ $ HH.ClassName "quote-box" ]
            [ HH.p_ [ HH.text "Click the button to discover a quote!" ] ]
        Just (Quote quote) ->
          HH.div 
            [ HP.class_ $ HH.ClassName "quote-box active" ]
            [ HH.blockquote 
                [ HP.class_ $ HH.ClassName "quote-text" ]
                [ HH.text $ "\"" <> quote.text <> "\"" ]
            , HH.div 
                [ HP.class_ $ HH.ClassName "quote-attribution" ]
                [ HH.cite_ [ HH.text $ "— " <> quote.author ]
                , case quote.year of
                    Just year -> HH.span [ HP.class_ $ HH.ClassName "quote-year" ] [ HH.text $ " (" <> show year <> ")" ]
                    Nothing -> HH.text ""
                ]
            , HH.div 
                [ HP.class_ $ HH.ClassName "quote-meta" ]
                [ HH.span 
                    [ HP.class_ $ HH.ClassName "category-tag" ]
                    [ HH.text quote.category ]
                , HH.div 
                    [ HP.class_ $ HH.ClassName "tags" ]
                    (map renderTag quote.tags)
                ]
            ]
    , HH.div 
        [ HP.class_ $ HH.ClassName "quote-actions" ]
        [ HH.button 
            [ HE.onClick \_ -> GetRandomQuote
            , HP.class_ $ HH.ClassName "get-quote-btn"
            ]
            [ HH.text "✨ Get Random Quote" ]
        ]
    ]

renderTag :: forall slots. String -> H.ComponentHTML Action slots Aff
renderTag tag =
  HH.span [ HP.class_ $ HH.ClassName "tag" ] [ HH.text tag ]

renderStats :: forall slots. State -> H.ComponentHTML Action slots Aff
renderStats state =
  let filteredQuotes = getFilteredQuotes state
      totalQuotes = length state.allQuotes
      filteredCount = length filteredQuotes
  in
  HH.div 
    [ HP.class_ $ HH.ClassName "stats-section" ]
    [ HH.p_ 
        [ HH.text $ "Showing " <> show filteredCount <> " of " <> show totalQuotes <> " quotes" ]
    ]

component :: forall query input output. H.Component query input output Aff
component = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval $ H.defaultEval 
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  }
  where
    initialState = 
      { currentQuote: Nothing
      , allQuotes: []
      , categories: []
      , authors: []
      , selectedCategory: Nothing
      , selectedAuthor: Nothing
      , searchQuery: ""
      , showFilters: false
      , loading: false
      , error: Nothing
      }

main :: Effect Unit
main = HA.runHalogenAff do
  _ <- HA.awaitBody
  app <- HA.selectElement (QuerySelector "#app")
  case app of
    Just element -> void $ runUI component unit element
    Nothing -> pure unit
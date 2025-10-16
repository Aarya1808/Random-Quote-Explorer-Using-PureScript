module Gemini.Types where

import Prelude
import Data.Maybe (Maybe)

-- A single quote
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

-- Category info
newtype Category = Category
  { name :: String
  , description :: String
  , color :: String
  }

-- Author info
newtype Author = Author
  { name :: String
  , bio :: String
  , birth_year :: Int
  , quote_count :: Int
  }

-- The whole database
newtype QuoteDatabase = QuoteDatabase
  { quotes :: Array Quote
  , categories :: Array Category
  , authors :: Array Author
  }

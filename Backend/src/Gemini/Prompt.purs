module Gemini.Prompt where

import Prelude
import Data.String (replace, Pattern(..), Replacement(..))


buildPrompt :: Int -> String
buildPrompt n =
  let
    basePrompt =
      "Generate " <> show n <> " random quotes. " <>
      "Each quote should have: id, text, author, category, year, source, tags (array). " <>
      "Return the result as valid JSON matching the structure: " <>
      "{ \"quotes\": [...], \"categories\": [...], \"authors\": [...] }"
  in

    replace (Pattern "\n") (Replacement " ") basePrompt

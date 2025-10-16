module Gemini.Prompt where

import Prelude
import Data.String (replace, Pattern(..), Replacement(..))

-- buildPrompt n generates the instructions for Gemini AI
buildPrompt :: Int -> String
buildPrompt n =
  let
    basePrompt =
      "Generate " <> show n <> " random quotes. " <>
      "Each quote should have: id, text, author, category, year (optional), source, tags (array). " <>
      "Return the result as valid JSON matching the structure: " <>
      "{ \"quotes\": [...], \"categories\": [...], \"authors\": [...] }"
  in
    -- optional: replace newlines with spaces for cleaner sending
    replace (Pattern "\n") (Replacement " ") basePrompt

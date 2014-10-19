module Main where

import Text.Show.Pretty
import Language.EventBased.Lexer

main = interact (ppShow . tokenize)

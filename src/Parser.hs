module Main where

import Text.Show.Pretty
import Language.EventBased.Lexer
import Language.EventBased.Parser

main = interact (ppShow . progParse . tokenize)

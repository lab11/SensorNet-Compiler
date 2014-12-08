module Main where

import Text.Show.Pretty
import Language.EventBased.Lexer
import Language.EventBased.Parser
import Language.IR

main = interact (ppShow . fromEventBased . progParse . tokenize)

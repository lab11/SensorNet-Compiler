module Main where

import Text.Show.Pretty
import Language.EventBased.Lexer
import Language.EventBased.Parser
import qualified Language.IR.FromEventBased as IR

main = interact (ppShow . IR.fromEventBased . progParse . tokenize)

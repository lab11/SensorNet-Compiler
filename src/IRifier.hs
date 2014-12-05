module Main where

import Text.Show.Pretty
import Language.EventBased.Lexer
import Language.EventBased.Parser
import Language.IR
import Language.IR.Analyses.TypeInference

main = interact (ppShow . infer . fromEventBased . progParse . tokenize)

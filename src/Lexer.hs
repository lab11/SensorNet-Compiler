module Main where

import Language.EventBased.Lexer
import Language.EventBased.Parser

main = interact (show . tokenize)

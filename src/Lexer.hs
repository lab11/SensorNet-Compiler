
module Main where

import Language.EventBased.Lexer

main = interact (show . tokenize)

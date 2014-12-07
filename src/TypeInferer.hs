module Main where

import Text.Show.Pretty
import Language.EventBased.Lexer
import Language.EventBased.Parser
import Language.IR
import Language.IR.Analyses.TypeInference
import System.Environment   
import System.IO

main = 
  do args <- getArgs
     let hf = head args
     let pf = args !! 1 
     si <- readFile pf 
     inferedTypes <- inferTypes ((fromEventBased . progParse . tokenize) si) hf
     (putStr . ppShow) inferedTypes

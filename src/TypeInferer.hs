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
     let hf = args !! 0
     let pf = args !! 1
     headers <- parseHeaderFile hf 
     let funcEnv = genFEnv headers
     putStrLn $ (ppShow funcEnv) ++ "\n\n"
     progText <- readFile pf 
     let prog  = (fromEventBased . progParse . tokenize) progText
     let types = inferTypes prog funcEnv 
     putStrLn $ ppShow types


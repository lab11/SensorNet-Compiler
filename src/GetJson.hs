module Main where

import Text.Show.Pretty
import Language.EventBased.Lexer
import Language.EventBased.Parser
import qualified Data.Map as Map
import Language.IR
import Language.IR.Analyses.TypeInference
import Language.IR.ToC
import System.Environment   
import System.IO

main = 
  do args <- getArgs
     let hf = args !! 0
     let pf = args !! 1
     headers <- parseHeaderFile hf 
     let funcEnv = genFEnv headers
     progText <- readFile pf 
     let prog  = (fromEventBased . progParse . tokenize) progText
     let types = inferTypes prog funcEnv
     let tmap = (Map.map head types) 
     putStrLn $ tableinfoToJSON ( genTableinfo prog tmap)


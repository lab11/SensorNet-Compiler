module Main where

import Language.EventBased.Lexer
import Language.EventBased.Parser
import Language.IR
import Language.IR.Analyses.CFGraph
import System.Environment   
import System.IO

{-
	Block Profusion Analysis
	e.g.	Sensornet-ProfusionChecker ?.txt
-}

main = 
  do args <- getArgs
     let pf = args !! 0 
     si <- readFile pf 
     profusify ((fromEventBased . progParse . tokenize) si)

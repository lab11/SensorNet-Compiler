module Main where

import Language.EventBased.Lexer
import Language.EventBased.Parser
import Language.IR
import Language.IR.Analyses.CFGraph
import System.Environment   
import System.IO

{-
	Outputs dot file (viewable with GraphViz)
	e.g.	Sensornet-CFGrapher ?.txt > ?.dot
	    	dot -Tpng ?.dot > ?.png
-}

main = 
  do args <- getArgs
     let pf = args !! 0 
     si <- readFile pf 
     dotify ((fromEventBased . progParse . tokenize) si)

module Main where

import Language.EventBased.Lexer
import Language.EventBased.Parser
import Language.IR
import Language.IR.Analyses.CFGraph
import System.Environment   
import System.IO

{-
	Outputs dot file (viewable with GraphViz)
	e.g.	Sensornet-CFGrapher > test.dot
	    	dot -Tpng test.dot > test.png
-}

main = dotify (fromEventBased . progParse . tokenize)

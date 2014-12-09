{-# LANGUAGE TemplateHaskell, ParallelListComp #-}

module Language.IR.Analyses.CFGraph where

import Language.IR
import Language.IR.IR
import Data.Map (Map) 
import qualified Data.Map as Map
import Language.EventBased.Parser (Interval,Email,BinOp,UnOp)
import Control.Lens
import Data.Text.Lazy (Text,pack,unpack)
import Data.Graph.Inductive
import Data.GraphViz hiding (Str,Int)
import Data.GraphViz.Printing
import Data.GraphViz.Attributes.Complete hiding (Int)

{-
	Outputs dot file (viewable with GraphViz)
	e.g.	Sensornet-CFGrapher ?.txt > ?.dot
	    	dot -Tpng ?.dot > ?.png
-}

bID (BlockID s) = s

eID (EventID s) = s

getEventMap = view rules

getIDs prog = Map.fromList (zip ([eID (fst c) | c<-a]++[bID (fst c) | c<-b]) [1..(length a)+(length b)])
			where 
				a = Map.toList (getEventMap prog)
				b = Map.toList (getBlockMap prog)


getMap prog = [(c Map.! (eID (fst d)), c Map.! (bID e), pack "") | d <- a, e<-snd d] 
			++ [(c Map.! (bID (fst d)), c Map.! (bID f), pack "") | d<-b, e<-snd d, f<-filter (/=BlockID "") (getSimultIfBlocks e)]
			where 
				a = Map.toList (getEventMap prog)
				b = Map.toList (getBlockMap prog)
				c = getIDs prog

blockProfuseCheck prog = filter (not . null) [esp c d a | c<-b, d<-b, c/=d]
	where
		a = graphify prog
		b = findLoops (a)

graphify :: Program -> Gr Text Text
graphify prog = mkGraph [(snd a, pack (fst a)) | a<-(Map.toList (getIDs prog))] (getMap prog)

findLoops :: Gr a b -> [Node]
findLoops a = [b | b <- nodes a, b `elem` (suc a b) ]

params :: GraphvizParams n Text Text () Text
params = nonClusteredParams { globalAttributes = ga, fmtNode = fn, fmtEdge = fe }
  where
    ga = [	GraphAttrs [ BgColor [toWColor Transparent] ],
			NodeAttrs [	Shape BoxShape, PenWidth 0.0, FillColor [toWColor  Gray], Style [SItem Filled []] ] ]
    fn (n,l) = [(Label . StrLabel) l]
    fe (f,t,l) = [(Label . StrLabel) l]

visualize = graphToDot params

dotify prog = putStr $ unpack $ renderDot $ toDot (visualize (graphify prog))

profusify prog = putStr ("Nodes: " ++ b ++ (if (null a) then "\nPASS\n" else "\nERROR - Paths between loops: " ++ a ++ "\n"))
	where
		a = show (blockProfuseCheck prog)
		b = show (labNodes (graphify prog))
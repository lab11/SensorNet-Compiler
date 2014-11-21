module Language.IR.FromEventBased (
) where

import Language.IR.IR 
import qualified Language.EventBased.Parser as P 
import Data.Map (Map) 
import qualified Data.Map as Map
import Language.EventBased.Parser (Interval,Email,BinOp,UnOp)
import Data.Time.LocalTime (LocalTime)



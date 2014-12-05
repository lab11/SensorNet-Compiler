module Language.IR.Analyses.TypeInference where

import Language.IR
import Language.C
import Language.C.System.GCC
import Data.Map (Map)
import qualified Data.Map as Map
import Language.EventBased.Parser (Interval,Email,BinOp(..),UnOp)
import Language.IR.IR
import Data.Time.LocalTime (LocalTime)
import Data.List as List
import Control.Lens



{-
data Instruction = SimultAt WaitType Time [BlockID]
| Store VarID Value
| Send Email Value
| Gather TableID [(FieldID,Value)]
| Call StoReg ExternCall [Value]
| Concat StoReg [Value]
| If Value (Maybe BlockID) (Maybe BlockID)
| BinaryOp StoReg BinOp Value Value
| UnaryOp StoReg UnOp Value
deriving (Show,Read,Eq,Ord)
-}

inferTypes :: Program -> FilePath -> IO (Map DataID CTypeSpec) 
inferTypes = error "Unimplemented" 

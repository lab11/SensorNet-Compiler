{-# LANGUAGE TemplateHaskell #-}

module Language.IR.ToC where

import Language.IR
import Language.C
import Language.C.System.GCC
import Text.Show.Pretty (ppShow)
import Language.C.Data.Ident
import Data.Map (Map)
import qualified Data.Map as Map
import Language.EventBased.Parser (Interval,Email,BinOp(..),UnOp(..))
import Language.IR.IR
import Data.Time.LocalTime (LocalTime)
import Data.List
import Data.String.Here
import Data.Maybe 
import Control.Monad.State
import Control.Monad.Trans (lift) 
import Control.Lens
import Language.IR.Analyses.TypeInference

toC :: Map String Type -> Map DataID Type -> Program -> String
toC fEnv tEnv prog = error "Unimplemented"

data Env = Env {
  _f :: Map String Type, 
  _d :: Map DataID Type,
  _p :: Program,
  _output :: Map String String  -- Map of Function Names to their contents
}

type Generator = State Env 

makeLenses ''Env

(+/+) :: String -> String -> String 
a +/+ b = a ++ "/n" ++ b

--convertEvent :: Event -> EventID -> Generator () 

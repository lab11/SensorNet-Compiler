{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.IR.ToC where

import Language.IR
import Language.C
import Language.C.System.GCC
import Text.Show.Pretty (ppShow)
import Language.C.Data.Ident
import Data.Map (Map)
import qualified Data.Map as Map
import Language.EventBased.Parser (Interval(..),Email(..),BinOp(..),UnOp(..))
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
  _output :: Map String [String],  -- Map of Function Names to their contents
  _semCounter :: Int
}

onto i = (at i).(non [])  

type Generator = State Env 

makeLenses ''Env

convertEvent :: Event -> EventID -> Generator () 
convertEvent Boot          (EventID n) = 
  output.onto "init" <>= [[i|${n}();|]]
convertEvent (Interrupt p) (EventID n) = 
  output.onto "init" <>= [[i|schedule_interrupt(&${n},${p});|]]

convertRule :: EventID -> [BlockID] -> Generator ()
convertRule (EventID e) bl = mapM_ addBlock bl
  where addBlock (BlockID b) = output.onto e <>= [[i|${b}();|]]

convertBlock :: BlockID -> Block -> Generator () 
convertBlock (BlockID b) il =
  do code <- mapM convertInst il 
     output.onto b <>= concat code 

--- START OF convertInst
convertInst :: Instruction -> Generator [String]

convertInst (SimultAt Waiting Now bl) = 
  do semNum <- semCounter %%= (\ n -> (n,n + 1))
     spawns <- mapM (getBlock semNum) bl 
     return $ spawns ++ [[i|join(NULL,${semNum},0);|]]
  where getBlock n (BlockID f) = return [i|spawn(&${f},${n});|]
 
convertInst (SimultAt Waiting (Abs _) _) = error "Invalid SimultAt"

convertInst (SimultAt Waiting (Rel _) _) = error "Invalid SimultAt"

convertInst (SimultAt Not_Waiting (Rel (Interval s)) bl) =
  mapM getBlock bl
  where getBlock (BlockID b) = return [i|schedule_absolute(&${b},${s});|]
  
convertInst (Store (VarID n) v) = 
  return [[i|${n} = ${toCVal v};|]]

convertInst (Send (Email e) v) = 
  return [[i|send_email(${show e},${toCVal v},1024);|]]

{-
convertInst (Gather t fl) = 
  do tableNum <- uses program.tables $ Map.lookupIndex t
     flush <- [[i|flush_buffer(${tableNum});|]]
     collects <- mapM (getStore tableNum)
  where getStore n (f,v) = 
          do fNum <- uses program.tables.at(t). $ List.elemIndex f
             return [[i|store_value(n,
-}
--- END OF convertInst

class ToCVal a where 
  toCVal :: a -> String 

instance ToCVal Value where
  toCVal (Reg (RegID r)) = r
  toCVal (Var (VarID v)) = v
  toCVal (Lit l)         = toCVal l

instance ToCVal Literal where 
  toCVal (Str s) = show s 
  toCVal (Flt f) = show f
  toCVal (Int i) = show i
  toCVal (Bool True) = "((bool_t) 0)" 
  toCVal (Bool False) = "((bool_t) 1)" 

class ToCBufLen a where 
  toCBufLen :: a -> Generator String 

instance ToCBufLen Value where
  toCBufLen (Reg r) = toCBufLen r
  toCBufLen (Val v) = toCBufLen v
  toCBufLen (Lit l) = toCBufLen l

instance ToCBufLen Literal where
  toCBufLen (Str s)  = 
    do sBuf <- toCBufLen StringT
       return $ "(" ++ sBUf ++ "* (" ++ (show $ 1 + (List.length s)) ++ "))"
  toCBufLen (Flt _)  = toCBufLen FloatT
  toCBufLen (Int _)  = toCBufLen IntT
  toCBufLen (Bool _) = toCBufLen BoolT

instance ToCBufLen Type where
  toCBufLen StringT     = return "sizeof(char)"
  toCBufLen IntT        = return "sizeof(int)"
  toCBufLen FLoatT      = return "sizeof(float)"
  toCBufLen VoidT       = error "Tried to get the size of a void variable" 
  toCBufLen IntervalT   = return "sizeof(interval_t)" 
  toCBufLen TimeT       = return "sizeof(time_t)" 
  toCBufLen BoolT       = return "sizeof(bool_t)" 
  toCBufLen SizeT       = return "sizeof(size_t)"
  toCBufLen (FuncT _ _) = error "Tried to get size of a function" 


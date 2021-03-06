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
import Language.IR.Analyses.TypeInference hiding (getType)


data Env = Env {
  _f :: Map String Type, 
  _d :: Map DataID Type,
  _p :: Program,
  _output :: Map String [String],  -- Map of Function Names to their contents
  _global :: [String],
  _semCounter :: Int,
  _concatCounter :: Int
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

convertHeader :: Generator ()
convertHeader =
  do o <- use output
     let h = map genHeader (Map.keys o)
     global <>= [[i| // Header Declarations |]]
     global <>= h
     global <>= [""]
     where genHeader s = [i|void ${s}();|]

convertGlobals :: Generator ()
convertGlobals = 
  do dat <- use d
     allocs <- mapM generateDecl 
                    ((Map.elems . (Map.mapMaybeWithKey filterGlobals)) dat)
     global <>= [[i| // Global Variables |]]
     global <>= allocs
     global <>= [""]
     where filterGlobals (VarName d) _ = Just d
           filterGlobals _           _ = Nothing 
           generateDecl gv@(VarID v)   = 
             do t <- getType gv 
                case t of 
                  StringT -> return [i|char *${v};|]
                  _       -> do t <- toCType gv
                                return [i|${t} ${v};|] 
               
convertFunction :: String -> [String] -> String
convertFunction n c = [i|
void ${n}(){
${unlines (map ("  " ++) c)}}|]

header :: String
header = [i|
/* This is an autogenerated file - Do not edit it */

#define NULL 0
|] 

convertProgram :: Generator String
convertProgram = 
  do evs <- use $ p.events
     mapM_ (uncurry convertEvent) (Map.toList evs)
     rls <- use $ p.rules 
     mapM_ (uncurry convertRule) (Map.toList rls)
     blks <- use $ p.blocks
     mapM_ (uncurry convertBlock) (Map.toList blks)
     convertHeader
     convertGlobals 
     o <- use output
     g <- use global
     let rf = Map.mapWithKey convertFunction o
     return $ unlines ([header] ++ g ++ (Map.elems rf))

toC :: Map String Type -> Map DataID Type -> Program -> String
toC fEnv tEnv prog = evalState convertProgram nEnv
  where nEnv = Env fEnv tEnv prog Map.empty [] 0 0



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
  where getBlock (BlockID b) = return [i|schedule_relative(&${b},${s});|]
  
convertInst (Store (VarID n) v) =
  do t <- getType v
     case t of 
        StringT -> return [[i|strncpy(${toCVal v},${n},1024);|]]
        _       -> return [[i|${n} = ${toCVal v};|]]

convertInst (Send (Email e) v) = 
  return [[i|send_email(${show e},${toCVal v},1024);|]]

convertInst (Gather t fl) = 
  do tableNum <- uses (p.tables) $ fromJust . (Map.lookupIndex t)
     let flush = [[i|flush_buffer(${tableNum});|]]
     collects <- mapM (getStore tableNum) fl
     let send = [[i|finish_record(${tableNum});|]]
     return $ flush ++ collects ++ send 
  where getStore n (f,v) = 
          do tList <- use $ p.tables.at(t) 
             let fNum = (fromJust .(elemIndex f) . fromJust) tList
             bufL <- toCBufLen v
             return [i|store_value(${n},${fNum},&${toCVal v},${bufL});|]

convertInst (Call s (ExternCall e) vl) = 
  do sts <- toCSto s
     let vlt = (concat . (intersperse ",") . (map toCVal)) vl
     return [[i|${sts} ${e}(${vlt});|]]

convertInst (If v mb1 mb2) =
  do let b1 = case mb1 of 
                Just (BlockID b1id) -> [i| ${b1id}();|]
                Nothing -> ""
     let b2 = case mb2 of 
                Just (BlockID b2id) -> [i| ${b2id}();|]
                Nothing -> ""
     let vs = toCVal v 
     return [[i|if(${vs}){|],b1,"} else {",b2,"}"]

convertInst (BinaryOp s bo v1 v2) = 
  do sts <- toCSto s 
     let v1s = toCVal v1 
     let v2s = toCVal v2 
     let bs = toCBinOp bo
     return [[i|${sts}((${v1s})${bs}(${v2s}));|]]

convertInst (UnaryOp s Logical_Not v) =
  do sts <- toCSto s 
     return [[i|${sts}(!(${toCVal v});|]]

convertInst (Concat s vl) = 
  do sts <- toCSto s
     cnt <- concatCounter %%= (\ n -> (n,n + 1))
     let init = [[i|char _b${cnt}[1024];|],
                 [i|char * _t${cnt} = _b${cnt};|]]
     concats <- mapM (addSegment cnt) vl 
     let cleanup = [[i|_t${cnt} = '\0';|],
                    [i|${sts} _b${cnt};|]]
     return $ init ++ concats ++ cleanup
     where addSegment n v = 
              do t <- getType v
                 call <- getStringCoerce t 
                 let h = [i|_t${n}|]
                 let p = [i|_b${n}|]
                 return [i|${h} = ${call}(${toCVal v},${h},1024-(${h}- ${p}));|]


convertInst s = error $ "nonExhaustive Pattern match on : " ++ (ppShow s)
--- END OF convertInst

getStringCoerce :: Type -> Generator String 
getStringCoerce t = 
  do m <- use f
     return $ (head . Map.keys . Map.filterWithKey filter) m 
     where filter k (FuncT StringT (b:_) ) = 
              ("string_coerce" `isPrefixOf` k) && (b == t)
           filter _ _ = False 

class ToCVal a where 
  toCVal :: a -> String 

instance ToCVal Value where
  toCVal (Reg r) = toCVal r
  toCVal (Var v) = toCVal v
  toCVal (Lit l) = toCVal l

instance ToCVal Literal where 
  toCVal (Str s) = show s 
  toCVal (Flt f) = [i|((float) ${show f})|]
  toCVal (Int i) = show i
  toCVal (Bool True) = "((bool_t) 0)" 
  toCVal (Bool False) = "((bool_t) 1)" 

instance ToCVal RegID where 
  toCVal (RegID r) = r

instance ToCVal VarID where
  toCVal (VarID v) = v

class ToCBufLen a where 
  toCBufLen :: a -> Generator String 

instance ToCBufLen Value where
  toCBufLen (Reg r) = toCBufLen r
  toCBufLen (Var v) = toCBufLen v
  toCBufLen (Lit l) = toCBufLen l

instance ToCBufLen Literal where
  toCBufLen (Str s)  = 
    do sBuf <- toCBufLen StringT
       return $ "(" ++ sBuf ++ "* (" ++ (show $ 1 + (length s)) ++ "))"
  toCBufLen (Flt _)  = toCBufLen FloatT
  toCBufLen (Int _)  = toCBufLen IntT
  toCBufLen (Bool _) = toCBufLen BoolT

instance ToCBufLen Type where
  toCBufLen VoidT       = error "Tried to get the size of a void variable" 
  toCBufLen (FuncT _ _) = error "Tried to get size of a function" 
  toCBufLen t           = 
    do ty <- toCType t
       return [i|sizeof(${ty})|]

instance ToCBufLen DataID where
  toCBufLen dat = 
    do t <- use $ d.at dat
       case t of 
          Just StringT -> do sn <- toCBufLen StringT 
                             return $ "(" ++ sn ++ " * 1024)" 
          Just t -> toCBufLen t
          Nothing -> error "Trying to get type of var with no type specified"

instance ToCBufLen VarID where 
  toCBufLen v = toCBufLen (VarName v) 

instance ToCBufLen RegID where
  toCBufLen r = toCBufLen (RegName r) 

class ToCType a where
  toCType :: a -> Generator String

instance ToCType Type where 
  toCType StringT     = return "char" 
  toCType IntT        = return "int" 
  toCType FloatT      = return "float" 
  toCType VoidT       = return "void" 
  toCType IntervalT   = return "interval_t" 
  toCType TimeT       = return "time_t" 
  toCType BoolT       = return "bool" 
  toCType SizeT       = return "size_t" 
  toCType (FuncT _ _) = error "Can't generate type for function types"

instance ToCType DataID where
  toCType did = 
    do t <- use $ d.at did
       case t of 
          Just ty -> toCType ty
          Nothing -> error $ "no type info available for " ++ (show did)

instance ToCType RegID where
  toCType r = toCType (RegName r)

instance ToCType VarID where 
  toCType v = toCType (VarName v)

toCSto :: StoReg -> Generator String
toCSto Null = return "" 
toCSto (Register r) =
  do t <- getType r
     ts <- toCType t 
     let name = toCVal r
     case t of 
       VoidT -> return ""
       _     -> return [i|${ts} ${name} = |] 
  
toCBinOp :: BinOp -> String 
toCBinOp Logical_Or = "||"	
toCBinOp Logical_Xor = "^"
toCBinOp Structural_Equality = "=="
toCBinOp Greater_Than = ">"
toCBinOp Greater_Than_Equals = ">=" 
toCBinOp Less_Than = "<"
toCBinOp Less_Than_Equals = "<=" 
toCBinOp String_Append = error "Shouldn't be called here"  
toCBinOp Add = "+"
toCBinOp Subtract = "-" 
toCBinOp Multiply = "*" 
toCBinOp Divide = "/"
toCBinOp Logical_And = "&&"

class CanGetType a where
  getType :: a -> Generator Type

instance CanGetType DataID where
  getType p = use $ d.at p.(non (error "foo"))

instance CanGetType VarID where 
  getType v = getType (VarName v) 

instance CanGetType RegID where 
  getType r = getType (RegName r) 

instance CanGetType FieldID where 
  getType f = getType (FldName f) 

instance CanGetType Literal where 
  getType (Str _) = return StringT
  getType (Flt _) = return FloatT
  getType (Int _) = return IntT
  getType (Bool _) = return BoolT

instance CanGetType Value where
  getType (Reg r) = getType r
  getType (Var v) = getType v
  getType (Lit l) = getType l

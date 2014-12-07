{-# LANGUAGE TemplateHaskell #-}

module Language.IR.Analyses.TypeInference where

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
import Data.Maybe 
import Control.Monad.State
import Control.Monad.Trans (lift) 
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

data Type = StringT
          | IntT
          | FloatT 
          | VoidT
          | IntervalT 
          | TimeT
          | SizeT
          | BoolT
          | FuncT Type [Type]
          deriving (Eq,Show,Read,Ord)

typeUniverse :: [Type]
typeUniverse = [StringT,IntT,FloatT,VoidT,IntervalT,TimeT,SizeT,BoolT]

numericTypes :: [Type] 
numericTypes = [IntT,FloatT]

inferTypes :: Program -> FilePath -> IO (Map DataID [Type]) 
inferTypes prog file = 
  do translationUnit <- parseHeaderFile file
     let fEnv = genFEnv translationUnit 
     putStrLn . (++ "\n\n\n") . ppShow $ fEnv -- TODO : Remove debug statements 
     return $ inferProg fEnv prog

 -- File IO --

parseHeaderFile :: FilePath -> IO CTranslUnit
parseHeaderFile input_file =
  do parse_result <- parseCFile(newGCC "gcc") Nothing ["-U__BLOCKS__"] input_file
     case parse_result of
       Left parse_err -> error (show parse_err)
       Right ast      -> return ast

 -- Function Type Extraction -- 

genFEnv :: CTranslUnit -> Map String Type 
genFEnv (CTranslUnit ds _) = Map.fromList $ mapMaybe getDef ds

getDef :: CExtDecl -> Maybe (String,Type)
getDef (CDeclExt (CDecl rts ((Just s,Nothing,Nothing):[]) _)) =
  do rType <- getElemType rts 
     fName <- getName s
     pType <- getParameterType s 
     return (fName, FuncT rType pType)
getDef _ = Nothing

getElemType :: [CDeclSpec] -> Maybe Type 
getElemType [] = Nothing
getElemType (x:xs) = 
  case x of 
    CTypeSpec (CTypeDef (Ident "time_t" _ _) _)     -> Just TimeT
    CTypeSpec (CTypeDef (Ident "size_t" _ _) _)     -> Just SizeT
    CTypeSpec (CTypeDef (Ident "interval_t" _ _) _) -> Just IntervalT
    CTypeSpec (CTypeDef (Ident "bool" _ _) _)       -> Just BoolT
    CTypeSpec (CFloatType _)                        -> Just FloatT
    CTypeSpec (CIntType _)                          -> Just IntT
    CTypeSpec (CVoidType _)                         -> Just VoidT
    CTypeSpec (CCharType _)                         -> Just StringT
    _ -> getElemType xs 

getName :: CDeclr -> Maybe String 
getName (CDeclr (Just (Ident s _ _)) _ _ _ _) = Just s
getName _                                     = Nothing

getParameterType :: CDeclr -> Maybe [Type]
getParameterType (CDeclr _((CFunDeclr(Right(ls,_))_ _):_)  _ _ _) =
  if all isJust x then return $ catMaybes x else Nothing
    where x = map getDeclType ls
getParameterType _ = Nothing

getDeclType :: CDecl -> Maybe Type
getDeclType (CDecl rt _ _) = getElemType rt 
getDeclType _ = Nothing

 -- Type Inference -- 

data TypingState = TypingState {
  _isModified :: Bool,  -- This is also silly : find some other way to do this
  _typeMap :: Map DataID [Type],
  _env :: Map String Type,
  _prog :: Program
} deriving (Eq,Show,Read)

  -- Manually Make Lenses because template haskell or control.lens is broken? 
setIsModified :: TypingState -> Bool -> TypingState
setIsModified (TypingState _ m e p) i = TypingState i m e p

setTypeMap :: TypingState -> Map DataID [Type] -> TypingState
setTypeMap (TypingState i _ e p) m = TypingState i m e p 

setEnv :: TypingState -> Map String Type -> TypingState
setEnv (TypingState i m _ p) e = TypingState i m e p 

setProg :: TypingState -> Program -> TypingState
setProg (TypingState i m e _) p = TypingState i m e p 

typeMap :: Lens' TypingState (Map DataID [Type])
typeMap = lens _typeMap setTypeMap

isModified :: Lens' TypingState Bool 
isModified = lens _isModified setIsModified

env :: Lens' TypingState (Map String Type)
env = lens _env setEnv 

prog :: Lens' TypingState (Program)
prog = lens _prog setProg

type Typer = State TypingState 

newTypingState e p = TypingState False Map.empty e p

inferProg :: Map String Type -> Program -> Map DataID [Type]
inferProg e p = (execState runInference $ newTypingState e p)^.typeMap

runInference :: Typer () 
runInference =
  do isModified .= False 
--   s <- use $ typeMap
--   return $! (trace "test" ())
--   return $! (trace (ppShow s) ())
--   p <- use $ prog
--   return (trace (ppShow p) ())
     runInferencePass
     isMod <- use isModified 
     if isMod 
     then runInference
     else return ()
-- (\ x -> trace (ppShow x) x) 
runInferencePass :: Typer ()
runInferencePass = 
  do  instrs <- use $ prog.blocks
      mapM_ infCmd $ concat . Map.elems $ instrs
  where infCmd = inferInst

inferInst :: Instruction -> Typer ()

inferInst (SimultAt _ _ _) = return ()

inferInst (Store s v) = constrainPair s v

inferInst (Send _ v) = constrainType v [StringT]

inferInst (Gather _ fl) = mapM_ (uncurry constrainPair) fl

-- TODO : Right now it will assume nothing if there's no valid 
--        function type in the environment, but we probably should 
--        fail or something. 
inferInst (Call s e p) = 
  do mt <- getCallType e 
     case mt of
        Just (FuncT rt pt) -> 
          do constrainType s [rt] 
             mapM_ (\ (v,t)-> constrainType v [t]) (zip p pt) 
        _ -> return ()

inferInst (Concat s vl) = 
  do constrainType s [StringT]
     sct <- stringCoerceTypes
     mapM_ ((flip constrainType) sct) vl 

inferInst (If v _ _) = constrainType v [BoolT]

inferInst (BinaryOp s Logical_And v1 v2) = constrainBooleanOps s v1 v2
inferInst (BinaryOp s Logical_Or v1 v2) = constrainBooleanOps s v1 v2
inferInst (BinaryOp s Logical_Xor v1 v2) = constrainBooleanOps s v1 v2

inferInst (BinaryOp s Structural_Equality v1 v2) =
  do constrainType s [BoolT]
     constrainPair v1 v2

inferInst (BinaryOp s Greater_Than v1 v2) = constrainComparisonOps s v1 v2
inferInst (BinaryOp s Greater_Than_Equals v1 v2) = constrainComparisonOps s v1 v2
inferInst (BinaryOp s Less_Than v1 v2) = constrainComparisonOps s v1 v2
inferInst (BinaryOp s Less_Than_Equals v1 v2) = constrainComparisonOps s v1 v2

inferInst (BinaryOp s String_Append v1 v2) =
  do constrainType s  [StringT] 
     constrainType v1 [StringT] 
     constrainType v2 [StringT] 

inferInst (BinaryOp s Add v1 v2) = constrainArithmeticOps s v1 v2
inferInst (BinaryOp s Subtract v1 v2) = constrainArithmeticOps s v1 v2
inferInst (BinaryOp s Multiply v1 v2) = constrainArithmeticOps s v1 v2

inferInst (BinaryOp s Divide v1 v2) =
  do constrainType s  [FloatT] 
     constrainType v1 numericTypes 
     constrainType v2 numericTypes

inferInst (UnaryOp s Logical_Not v1) =
  do constrainType s  [BoolT] 
     constrainType v1 [BoolT] 

-- END OF inferInst :: Instruction -> Typer ()

constrainBooleanOps s v1 v2 = 
  do constrainType s  [BoolT] 
     constrainType v1 [BoolT] 
     constrainType v2 [BoolT] 

constrainComparisonOps s v1 v2 = 
  do constrainType s  [BoolT] 
     constrainType v1 numericTypes 
     constrainType v2 numericTypes

constrainArithmeticOps s v1 v2 = 
  do constrainType v1 numericTypes 
     constrainType v2 numericTypes
     constrainDirectional s v1 
     constrainDirectional s v2 

constrainPair :: (Typeable a,Typeable b) => a -> b -> Typer() 
constrainPair a b = 
  do constrainDirectional a b 
     constrainDirectional b a 

constrainDirectional :: (Typeable a,Typeable b) => a -> b -> Typer() 
constrainDirectional a b = 
  do aType <- getType a  
     constrainType b aType 

getCallType :: ExternCall -> Typer (Maybe Type)
getCallType (ExternCall n) = use $ env . at n

stringCoerceTypes :: Typer [Type] 
stringCoerceTypes = filterM canStringCoerce typeUniverse

canStringCoerce :: Type -> Typer Bool 
canStringCoerce t = 
  do m <- use env 
     return $ Map.empty /= Map.filterWithKey filter m 
     where filter k (FuncT StringT (b:_) ) = 
              ("string_coerce" `isPrefixOf` k) && (b == t)
           filter _ _ = False 
              
class Typeable a where
  getType :: a -> Typer [Type] 
  constrainType :: a -> [Type] -> Typer ()

instance Typeable DataID where
  getType d =
    do dt <- use $ typeMap.at d
       case dt of 
          Nothing -> return typeUniverse
          Just t  -> return t 
  constrainType d t = 
    do dt <- getType d
       let ct = intersect dt t 
       if ((ct \\ dt) /= [])
       then isModified .= True
       else return ()
       typeMap.at d .= Just ct

instance Typeable VarID where 
  getType v = getType (VarName v) 
  constrainType v t = constrainType (VarName v) t

instance Typeable RegID where 
  getType r = getType (RegName r) 
  constrainType r t = constrainType (RegName r) t

instance Typeable FieldID where 
  getType f = getType (FldName f) 
  constrainType f t = constrainType (FldName f) t

instance Typeable Literal where 
  getType (Str _) = return [StringT]
  getType (Flt _) = return [FloatT]
  getType (Int _) = return numericTypes
  getType (Bool _) = return [BoolT]
  constrainType _ _ = return ()

instance Typeable Value where 
  getType (Reg r) = getType r
  getType (Lit l) = getType l 
  getType (Var v) = getType v
  constrainType (Reg r) = constrainType r
  constrainType (Lit l) = constrainType l
  constrainType (Var v) = constrainType v

instance Typeable StoReg where 
  getType (Register r) = getType r
  getType Null         = return typeUniverse
  constrainType (Register r) v = constrainType r v
  constrainType Null         _ = return ()

module Language.IR.Analyses.TypeInference where

import Language.IR
import Language.C
import Language.C.System.GCC
import Language.C.Data.Ident
import Data.Map (Map)
import qualified Data.Map as Map
import Language.EventBased.Parser (Interval,Email,BinOp(..),UnOp)
import Language.IR.IR
import Data.Time.LocalTime (LocalTime)
import Data.List as List
import Data.Maybe (catMaybes,mapMaybe,isJust) 
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

inferTypes :: Program -> FilePath -> IO (Map DataID [Type]) 
inferTypes prog file = 
  do translationUnit <- parseHeaderFile file 
     return $ inferProg prog translationUnit

parseHeaderFile :: FilePath -> IO CTranslUnit
parseHeaderFile input_file =
  do parse_result <- parseCFile(newGCC "gcc") Nothing ["-U__BLOCKS__"] input_file
     case parse_result of
       Left parse_err -> error (show parse_err)
       Right ast      -> return ast

inferProg :: Program -> CTranslUnit -> Map DataID [Type]
inferProg prog headers = 
  do let env = genEnv headers 
     error "Not yet implemented." 

genEnv :: CTranslUnit -> Map String Type 
genEnv (CTranslUnit ds _) = Map.fromList $ mapMaybe getDef ds

getDef :: CExtDecl -> Maybe (String,Type)
getDef (CDeclExt (CDecl rts ((Just s,Nothing,Nothing):[]) _)) =
  do rType <- getType rts 
     fName <- getName s
     pType <- getParameterType s 
     return (fName, FuncT rType pType)

getDef _ = Nothing

getType :: [CDeclSpec] -> Maybe Type 
getType (x:xs) = 
  case x of 
    CTypeSpec (CTypeDef (Ident "time_t" _ _) _)     -> Just TimeT
    CTypeSpec (CTypeDef (Ident "size_t" _ _) _)     -> Just SizeT
    CTypeSpec (CTypeDef (Ident "interval_t" _ _) _) -> Just IntervalT
    CTypeSpec (CTypeDef (Ident "bool" _ _) _)       -> Just BoolT
    CTypeSpec (CFloatType _)                        -> Just FloatT
    CTypeSpec (CIntType _)                          -> Just IntT
    CTypeSpec (CVoidType _)                         -> Just VoidT
    CTypeSpec (CCharType _)                         -> Just StringT
    _ -> getReturnType xs 
getReturnType _ = Nothing

getName :: CDeclr -> Maybe String 
getName (CDeclr (Just (Ident s _ _)) _ _ _ _) = Just s
getName _                                     = Nothing

getParameterType :: CDeclr -> Maybe [Type]
getParameterType (CDeclr _ 
                   ((CFunDeclr 
                     (Right (ls ,_ )) _ _):_)  _ _ _) =
  do let x = map getDeclType ls
     if all isJust x
     then return $ catMaybes x
     else Nothing

getParameterType _ = Nothing

getDeclType :: CDecl -> Maybe Type
getDeclType (CDecl rt _ _) = getType rt 
getDeclType _ = Nothing


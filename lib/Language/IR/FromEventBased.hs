{-# LANGUAGE TemplateHaskell #-}

module Language.IR.FromEventBased (
) where

import Language.IR.IR 
import qualified Language.EventBased.Parser as P 
import Language.EventBased.Parser (actAssign,valAssign,rules)
import Data.Map (Map) 
import qualified Data.Map as Map
import Language.EventBased.Parser (Interval,Email,BinOp,UnOp)
import Data.Time.LocalTime (LocalTime)
import Control.Monad.State
import Control.Monad.Trans (lift) 
import Control.Lens 

-- Utility Types -- 

data ID = Block String
        | Table String 
        | Event String
        | Field String 
        | Register String
        | Variable String 
        deriving (Show,Read,Eq,Ord)

-- Types for the Transformer Monad --

data TransformState = TransformState { 
  _ast :: P.Program,
  _ir :: Program,
  _environment :: Map String ID, 
  _counter :: Int
} 

makeLenses ''TransformState

type Transformer = State TransformState

-- Functions for the Transformer Monad --

initTransformState :: P.Program -> TransformState
initTransformState s = TransformState s emptyProg Map.empty 0

getNextCounter :: Transformer Int 
getNextCounter = do i <- use counter
                    counter += 1 
                    return i 

-- Types for the BlockTransformer Monad --

data BlockState = BlockState {
  _workingBlock :: Block
}

makeLenses ''BlockState

type BlockTransformer = StateT BlockState Transformer

-- Functions for the BlockTransformer Monad --

initBlockState :: Block -> BlockState 
initBlockState b = BlockState b 

getNextCounterB :: BlockTransformer Int 
getNextCounterB = lift getNextCounter

getNextRegNameB :: BlockTransformer String
getNextRegNameB = do c <- getNextCounterB
                     return $ "reg_" ++ (show c) 

addInstruction :: Instruction -> BlockTransformer () 
addInstruction i = workingBlock %= (++ [i]) 

-- TODO : seriously is there a better way to do this? This is ugly as fuck. 
doWithinB :: Transformer m -> BlockTransformer m
doWithinB t = do s <- (lift get)
                 let (a,ns) = runState t s
                 lift (put ns)
                 return a

-- Lifted Versions of Infix operators -- 
--
infix 4 ^%=
a ^%= b = lift (a %= b)

-- Converter Functions --

fromEventBased :: P.Program -> Program 
fromEventBased p = (execState convert $ initTransformState p) ^. ir

-- Converter Monads --

convert :: Transformer () 
convert = do convertActAssigns 
             convertValAssigns 
             convertRules

-- Go through all of the action assignments, and convert each one into
--   a block in the IR.  
convertActAssigns :: Transformer ()
convertActAssigns = do l <- use $ ast . actAssign
                       mapM_ convertActAssign l
                                  
convertActAssign :: P.AAssign -> Transformer() 
convertActAssign (P.AAssign (P.ID i) b) = 
  do (BlockID bID) <- convertBlock ("act_assign_" ++ i) b
     environment %= Map.insert i (Block bID) 

convertBlock :: String -> [P.AExpr] -> Transformer BlockID
convertBlock s b = do let initState = initBlockState []
                      retState <- execStateT (mapM_ convertAExpr b) initState
                      c <- getNextCounter
                      let id = BlockID $ s ++ "_" ++ (show c)
                      let newBlock = retState ^. workingBlock
                      ir . blocks %= Map.insert id newBlock 
                      return id

convertAExpr :: P.AExpr -> BlockTransformer ()
convertAExpr (P.AEGather r s)  = convertAEGather r s 
convertAExpr (P.AESend e v)    = convertAESend e v 
convertAExpr (P.AEExec v)      = convertAEExec v
convertAExpr (P.AEIf v bt bf)  = convertAEIf v bt bf
convertAExpr (P.AEDo s)        = convertAEDo s
convertAExpr (P.AEVassign s v) = convertAEVassign s v

convertAEGather :: [P.Record] -> String -> BlockTransformer ()
convertAEGather r t = 
  do let bt = TableID t 
     dl <- mapM (convertRecord bt) r 
     let (blocks,fields) = unzip dl 
     addInstruction $ SimultAt Waiting Now blocks
     addInstruction $ Gather bt fields
     
convertRecord :: TableID -> P.Record -> BlockTransformer (BlockID,(FieldID,Value))
convertRecord t (P.Record v s) = 
  do c <- getNextCounterB 
     let vName = "_fld_var_" ++ s ++ (show c)
     let bName = "record_blk_" ++ s
     let fID   = (FieldID s) 
     ir . tables ^%= Map.insertWith (++) t [fID]
     bID <- doWithinB $ convertBlock bName [P.AEVassign (P.ID vName) v]
     return (bID,(fID,Var (VarID vName)))

convertAESend :: P.Email -> P.VExpr -> BlockTransformer ()
convertAESend e v = do sr <- convertVExpr v
                       addInstruction $ Send e sr

convertAEExec :: P.VExpr -> BlockTransformer ()
convertAEExec v = void $ convertVExpr v

convertAEDo :: P.ID -> BlockTransformer ()
convertAEDo (P.ID id) = 
  do (Just (Block bID)) <- lift $ uses environment (Map.lookup id) 
     addInstruction $ SimultAt Waiting Now [BlockID bID]

convertAEVassign :: P.ID -> P.VExpr -> BlockTransformer ()
convertAEVassign (P.ID id) v =
  do v <- convertVExpr v
     addInstruction $ Store (VarID id) v

-- TODO : This doesn't use the Maybe Feature at all add that 
convertAEIf :: P.VExpr -> [P.AExpr] -> [P.AExpr] -> BlockTransformer ()
convertAEIf v bt bf = 
  do vr <- convertVExpr v 
     c <- getNextCounterB
     let btName = "if_true_" ++ (show c) 
     btID <- doWithinB $ convertBlock btName bt 
     let bfName = "if_false_" ++ (show c) ++ "_"
     bfID <- doWithinB $ convertBlock bfName bf 
     addInstruction $ If vr (Just btID) (Just bfID)

convertVExpr :: P.VExpr -> BlockTransformer Value
convertVExpr v = error "Unimplemented" 

convertValAssigns :: Transformer ()
convertValAssigns = error "Unimplemented" 

convertRules :: Transformer ()
convertRules = error "Unimplemented" 


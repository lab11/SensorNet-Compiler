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
import Control.Lens 

-- Utility Types -- 

data ID = Block String
        | Table String 
        | Event String
        | Field String 
        | Reg String
        | Var String 
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

getNextCounterB :: BlockTransformer Int 
getNextCounterB = lift getNextCounter

-- Functions for the BlockTransformer Monad --

initBlockState :: Block -> BlockState 
initBlockState b = BlockState b 

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
  do bID <- convertBlock ("act_assign_" ++ i) b
     environment %= Map.insert i bID 

convertBlock :: String -> [P.AExpr] -> Transformer ID
convertBlock s b = do let initState = initBlockState []
                      retState <- execStateT (mapM_ convertAExpr b) initState
                      c <- getNextCounter
                      let id =  s ++ "_" ++ (show c)
                      let newBlock = retState ^. workingBlock
                      ir . blocks %= Map.insert (BlockID id) newBlock 
                      return $ Block id


convertAExpr :: P.AExpr -> BlockTransformer ()
convertAExpr = error "Unimplemented" 

convertValAssigns :: Transformer ()
convertValAssigns = error "Unimplemented" 

convertRules :: Transformer ()
convertRules = error "Unimplemented" 


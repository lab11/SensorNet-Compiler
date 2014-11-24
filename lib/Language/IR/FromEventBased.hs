{-# LANGUAGE TemplateHaskell #-}

module Language.IR.FromEventBased (
) where

import Language.IR.IR 
import qualified Language.EventBased.Parser as P 
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

-- Types for the BlockTransformer Monad --

data BlockState = BlockState {
  _currentBlock :: Block
}

makeLenses ''BlockState

type BlockTransformer = StateT BlockState Transformer

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

convertActAssigns :: Transformer ()
convertActAssigns = error "Unimplemented" 

convertValAssigns :: Transformer ()
convertValAssigns = error "Unimplemented" 

convertRules :: Transformer ()
convertRules = error "Unimplemented" 



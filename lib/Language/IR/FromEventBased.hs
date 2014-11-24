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
convert = error "unimplemented"

{-
-- Constructs the inital state for the transformation from an AST

-- The one this this module exports the converter function
  
-- Given a transformer loaded with the correct state this will
--   completely assemble the IR representation  
transform :: Transformer () 
transform = do convertActAssigns
               convertValAssigns
               convertRules 

-- Go through all the elements in an action assignment and make sure 
-- that they are converted to event blocks in the final tree  
convertActAssigns :: Transformer ()
convertActAssigns = do x <- get
                       let a = P.actAssign . ast $ x
                       if (a /= []) 
                          then do put (liftAST (P.liftActAssign tail) x)
                                  addActAssign (head a)
                                  convertActAssigns 
                          else return ()

addActAssign :: P.AAssign -> Transformer ()
addActAssign p = do let P.AAssign id block = p
                    
                    return ()

--convertBlock :: EventID -> P.Aexpr 

convertValAssigns :: Transformer ()
convertValAssigns = error "unimplemented"

convertRules :: Transformer ()
convertRules = error "unimplemented" 
-}

module Language.IR.IR (
) where

import Data.Map (Map) 
import qualified Data.Map as Map
import qualified Language.EventBased.Parser as P 
import Language.EventBased.Parser (Interval,Email,BinOp,UnOp)
import Data.Time.LocalTime (LocalTime)


{-
 - - Goal of IR design 
 -    - Simplicity for conversion to C
 -    - Simplicity for analyses
 -      - Type Checking / Inference 
 -      - Power Consumption Estimate 
 -      - Memory Use Bounds 
 -      - Infinite Loop Prevention. 
 -    - Flatten Structure 
 -    - Preserve Semantics 
 -      - incl. async gather acts. 
 -      - Arbitrarily complex gather acts 
 -    - Move to something closer to executable code
 -    - Make it easier to collate information (table entries, local variables,
 -       external calls, etc..)
 -    - Support future additions to the language with minimal changes 
 -}

newtype BlockID = BlockID String
newtype EventID = EventID String 

newtype TableID = TableID String
newtype FieldID = FieldID String 

newtype RegID = RegID String 
newtype VarID = VarID String 

data Literal = Str String
             | Flt Float
             | Int Int 
             | Bool Bool 

data Value = Reg RegID 
           | Lit Literal
           | Var VarID


newtype ExternCall = ExternCall String

data Program = Program {
  tables :: Map TableID [FieldID],
  rules :: Map EventID [BlockID], 
  blocks :: Map BlockID [Action]
} 

data Event = Boot 
           | Interrupt String
          
data Action = Simult [BlockID] 
            | Store VarID Value
            | Send Email Value
            | Gather TableID [(FieldID,Value)] 
            | Call RegID ExternCall [Value]
            | Concat RegID [Value] 
            | If RegID (Maybe BlockID) (Maybe BlockID)
            | BinaryOp RegID BinOp Value Value
            | UnaryOp RegID UnOp Value
          

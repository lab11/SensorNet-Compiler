module Language.IR.IR (
) where

import Data.Map (Map) 
import qualified Data.Map as Map
import Language.EventBased.Parser (Interval,Email,BinOp,UnOp)
import Data.Time.LocalTime (LocalTime)

{-
 - Goal of IR design 
    - Simplicity for conversion to C
    - Simplicity for analyses
      - Type Checking / Inference 
        - TODO : Get the Language.C libraries working so we can get type
                 info from header files.
      - Power Consumption Estimate 
      - Memory Use Bounds 
      - Infinite Loop Prevention unless timer spaced
      - Find upper bound on number of timers/sems needed 
    - Flatten Structure 
    - Preserve Semantics 
      - incl. async gather acts. 
      - Arbitrarily complex gather acts 
    - Move to something closer to executable code
    - Make it easier to collate information (table entries, local variables,
       external calls, etc..)
    - Support future additions to the language with minimal changes 
 -}

newtype BlockID = BlockID String  -- Type Safe Block Identifier
                deriving (Show,Read,Eq,Ord)
newtype EventID = EventID String  -- Type Safe Event Identifier
                deriving (Show,Read,Eq,Ord)

newtype TableID = TableID String  -- Type Safe Table Identifier
                deriving (Show,Read,Eq,Ord)
newtype FieldID = FieldID String  -- Type Safe Field Identifier
                deriving (Show,Read,Eq,Ord)

newtype RegID = RegID String      -- Local Register Identifier
              deriving (Show,Read,Eq,Ord)
newtype VarID = VarID String      -- Global Variable Identifier
              deriving (Show,Read,Eq,Ord)

data Literal = Str String         -- Literal Values
             | Flt Float
             | Int Int 
             | Bool Bool 
             deriving (Show,Read,Eq,Ord)

data StoReg = Register RegID      -- Registers we can store into, null just 
            | Null                --  discards the value.
            deriving (Show,Read,Eq,Ord)

data Value = Reg RegID            -- Values we can use as parameters
           | Lit Literal          --    instructions
           | Var VarID
           deriving (Show,Read,Eq,Ord)

newtype ExternCall = ExternCall String  -- TypeSafe External Call Identifier 
                   deriving (Show,Read,Eq,Ord)

data Time = Rel Interval 
          | Now
          deriving (Show,Read,Eq,Ord)

data Event = Boot                 -- Default system events (i.e not invoked by
           | Interrupt String     --    other blocks)
           deriving (Show,Read,Eq,Ord)

data WaitType = Waiting
              | Not_Waiting

-- Actions that we can take.
data Action = SimultAt WaitType Time [BlockID]
            | Store VarID Value
            | Send Email Value
            | Gather TableID [(FieldID,Value)] 
            | Call StoReg ExternCall [Value]
            | Concat StoReg [Value] 
            | If Value (Maybe BlockID) (Maybe BlockID)
            | BinaryOp StoReg BinOp Value Value
            | UnaryOp StoReg UnOp Value
            deriving (Show,Read,Eq,Ord)
         
type Block = [Action]

data Program = Program {
  tables :: Map TableID [FieldID],
  events :: Map Event EventID,
  rules :: Map EventID [BlockID], 
  blocks :: Map BlockID Block
} deriving (Show,Read,Eq)

{-
 
 There's a number of invariants that we need to preserve
  
 - No RegID may be repeated in a block 
 - No RegID may be used before it is defined in a block 
 - No RegID may be assigned more than once, unless it is 
 - Simult will run all the blocks and only continue its own 
   execution when that's done.
 - Simult and If must have at least one block to execute
 - Never use the String_Concat binop 
 - All values must be initialized before they are used.
 - Optional: Register names musn't be repeated across the whole program.
 - Optional: Don't do anything that would require constant propagation

 TODO : Write a "Program -> Bool" validity check function that makes sure 
        none of the above conditions are broken.

 TODO : Make sure we can Print the a Programin some human readable way


 -} 

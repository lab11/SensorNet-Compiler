module Language.IR.Analyses.TypeInference where

import Language.IR

{-}
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

infer :: Program -> Program
infer program = Program {
    _tables = _tables program,
    _events = _events program,
    _rules = _rules program,
    _blocks = fmap inferBlock (getBlockMap program)
  }

inferBlock :: Block -> Block
inferBlock block = fmap inferInstruction block

inferInstruction :: Instruction -> Instruction
{-
inferInstruction (Store varId value) =
inferInstruction (Call storeReg externFun args) =
inferInstruction (Concat storeReg values) =
inferInstruction (If value _ _) =
inferInstruction (BinaryOp storeReg op a b) =
inferInstruction (UnaryOp storeReg op a) =
-}
-- Everything else
inferInstruction s = id s

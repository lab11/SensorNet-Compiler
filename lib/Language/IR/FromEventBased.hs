{-# LANGUAGE TemplateHaskell #-}

module Language.IR.FromEventBased (
  fromEventBased
) where

import Language.IR.IR 
import qualified Language.EventBased.Parser as P 
import Language.EventBased.Parser (actAssign,valAssign)
import Data.Map (Map) 
import qualified Data.Map as Map
import Language.EventBased.Parser (Interval(..),Email(..),BinOp(..),UnOp(..))
import Data.Time.LocalTime (LocalTime)
import Control.Monad.State
import Control.Monad.Trans (lift) 
import Control.Lens 

-- Utility Types -- 

data ID = Block String
        | Table String 
        | Event String
        | Field String 
        | Rgstr String
        | Variable String 
        deriving (Show,Read,Eq,Ord)

-- Types for the Transformer Monad --

data TransformState = TransformState { 
  _ast :: P.Program,
  _ir :: Program,
  _environment :: Map String ID, 
  _counter :: Int,
  _currentRule :: Int   -- Haskell Version of "just stuff it in a global var"
                        --  TODO : fix it. 
} 

makeLenses ''TransformState

type Transformer = State TransformState

-- Functions for the Transformer Monad --

initTransformState :: P.Program -> TransformState
initTransformState s = TransformState s emptyProg Map.empty 0 1

getNextCounter :: Transformer Int 
getNextCounter = 
  do i <- use counter
     counter += 1 
     return i 

getNextReg :: Transformer RegID
getNextReg = 
  do c <- getNextCounter
     return $ RegID ("reg_" ++ (show c))


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

getNextRegB :: BlockTransformer RegID
getNextRegB = 
  do c <- getNextCounterB
     return $ RegID ("reg_" ++ (show c))

addInstruction :: Instruction -> BlockTransformer () 
addInstruction i = workingBlock %= (++ [i]) 

-- TODO : seriously is there a better way to do this? This is ugly as fuck. 
doWithinB :: Transformer m -> BlockTransformer m
doWithinB t = 
  do s <- (lift get)
     let (a,ns) = runState t s
     lift (put ns)
     return a

-- Lifted Versions of Infix operators -- 
--
infix 4 ^%=
a ^%= b = lift (a %= b)

-- Converter Functions --

fromEventBased :: P.Program -> Program 
fromEventBased p = (execState convert $ initTransformState p)^.ir

-- Converter Monads --

convert :: Transformer () 
convert = 
  do convertActAssigns 
     convertValAssigns 
     convertRules
    -- cleanBootEvent

-- Go through all of the action assignments, and convert each one into
--   a block in the IR.  
convertActAssigns :: Transformer ()
convertActAssigns =
  do l <- use $ ast.actAssign
     mapM_ convertActAssign l
                            
-- convert a single action assignment, add it to the environment map              
convertActAssign :: P.AAssign -> Transformer() 
convertActAssign (P.AAssign (P.ID i) b) = 
  do (BlockID bID) <- convertBlock ("act_assign_" ++ i) b
     environment %= Map.insert i (Block bID) 

convertBlock :: String -> [P.AExpr] -> Transformer BlockID
convertBlock s b = 
  do let initState = initBlockState []
     retState <- execStateT (mapM_ convertAExpr b) initState
     addBlock s $ retState ^. workingBlock

convertVExprToBlock :: String -> P.VExpr -> Transformer (BlockID,Value)
convertVExprToBlock s v =
  do let initState = initBlockState []
     (retVal,retState) <- runStateT (convertVExpr v) initState
     bID <- addBlock s $ retState ^. workingBlock 
     return (bID,retVal)

addBlock :: String -> Block -> Transformer BlockID 
addBlock n b = 
  do c <- getNextCounter
     let id = BlockID $ n ++ "_" ++ (show c)
     ir.blocks %= Map.insert id b
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
     
convertRecord:: TableID -> P.Record -> BlockTransformer (BlockID,(FieldID,Value))
convertRecord t (P.Record v s) = 
  do c <- getNextCounterB 
     let vName = "_fld_var_" ++ s ++ (show c)
     let bName = "record_blk_" ++ s
     let fID   = (FieldID s) 
     ir.tables ^%= Map.insertWith (++) t [fID]
     bID <- doWithinB $ convertBlock bName [P.AEVassign (P.ID vName) v]
     return (bID,(fID,Var (VarID vName)))

convertAESend :: P.Email -> P.VExpr -> BlockTransformer ()
convertAESend e v = 
  do sr <- convertVExpr v
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
convertVExpr (P.VEBinop P.String_Append v1 v2) = convertConcat v1 v2 
convertVExpr (P.VEBinop b v1 v2) = convertBinOp b v1 v2
convertVExpr (P.VEUnop u v) = convertUnOp u v 
convertVExpr (P.VECall fn p) = convertCall fn p 
convertVExpr (P.VEStr s)  = return $ Lit (Str s)
convertVExpr (P.VEInt i)  = return $ Lit (Int i)
convertVExpr (P.VEFlt f)  = return $ Lit (Flt f) 
convertVExpr (P.VEBool b) = return $ Lit (Bool b) 
convertVExpr (P.VEId i)   = return $ Var (VarID i)

convertConcat :: P.VExpr -> P.VExpr -> BlockTransformer Value 
convertConcat v1 v2 =
  do val1 <- aggregateConcat v1 
     val2 <- aggregateConcat v2 
     ro <- getNextRegB 
     addInstruction $ Concat (Register ro) (val1 ++ val2) 
     return (Reg ro)

aggregateConcat :: P.VExpr -> BlockTransformer [Value] 
aggregateConcat (P.VEBinop P.String_Append v1 v2) = 
  do val1 <- aggregateConcat v1 
     val2 <- aggregateConcat v2 
     return $ val1 ++ val2 

aggregateConcat v = 
  do val <- convertVExpr v
     return [val] 

convertBinOp :: P.BinOp -> P.VExpr -> P.VExpr -> BlockTransformer Value 
convertBinOp b v1 v2 =
  do val1 <- convertVExpr v1 
     val2 <- convertVExpr v2 
     ro <- getNextRegB
     addInstruction $ BinaryOp (Register ro) b val1 val2
     return (Reg ro)

convertUnOp :: P.UnOp -> P.VExpr -> BlockTransformer Value
convertUnOp u v = 
  do val <- convertVExpr v
     ro <- getNextRegB
     addInstruction $ UnaryOp (Register ro) u val
     return (Reg ro) 

convertCall :: String -> [P.VExpr] -> BlockTransformer Value
convertCall fn p = 
  do vals <- mapM convertVExpr p 
     ro <- getNextRegB
     addInstruction $ Call (Register ro) (ExternCall fn) vals 
     return (Reg ro) 

-- TODO : make use addToEvent
convertValAssigns :: Transformer ()
convertValAssigns = 
  do assigns <- use $ ast.valAssign
     c <- getNextCounter
     mainBlock <- convertBlock ("valAssigns_" ++ (show c)) assigns 
     addToEvent Boot mainBlock

convertRules :: Transformer ()
convertRules = 
  do r <- use $ ast . P.rules 
     mapM_ convertRule r

convertRule :: P.Rule -> Transformer () 
convertRule (P.Rule e as) =
  do c <- getNextCounter
     currentRule .= c 
     b <- convertBlock ("rule_" ++ (show c) ++ "_action") as 
     convertEExpr e b 

eventIDName :: Event -> EventID 
eventIDName Boot = EventID "on_boot"
eventIDName (Interrupt s) = EventID ("on_interrupt_" ++ s)

-- I am depending on the functional nature of eventIDName and the idempotency
-- of a partially curries call to Map.Insert. If anyone tries to add to these 
-- elements elsewhere I'm screwed. FIX ME 
addToEvent :: Event -> BlockID -> Transformer () 
addToEvent e b = 
  do let n = eventIDName e 
     ir.events %= Map.insert e n 
     ir.rules %= Map.insertWith (flip (++)) n [b]         

convertEExpr :: P.EExpr -> BlockID -> Transformer () 
convertEExpr (P.EVEvery i) b = convertEvery i b 
convertEExpr (P.EVStartingAt i l) b = convertStartingAt i l b 
convertEExpr (P.EVAfter i e) b = convertAfter i e b 
convertEExpr (P.EVInterrupt i) b = convertInterrupt i b
convertEExpr (P.EVCooldown e i) b = convertCooldown e i b 
convertEExpr (P.EVBegins v i) b = convertBegins v i b 
convertEExpr (P.EVEnds v i ) b = convertBegins (P.VEUnop P.Logical_Not v) i b 

convertEvery :: Interval -> BlockID -> Transformer ()
convertEvery i b =
  do bID <- convertPeriodic "every_bootstrap" i Now b
     addToEvent Boot bID 

convertStartingAt :: Interval -> LocalTime -> BlockID -> Transformer ()
convertStartingAt i l b =
  do bID <- convertPeriodic "starting_bootstrap" i (Abs l) b
     addToEvent Boot bID

convertPeriodic :: String -> Interval -> Time -> BlockID -> Transformer BlockID
convertPeriodic slug i s b = 
  do n <- use $ currentRule 
     let bsn = "rule_" ++ (show n) ++ "_" ++  slug
     let bsb = [SimultAt Waiting Now [b]]
     bID <- addBlock bsn bsb
     ir.blocks %= Map.insertWith (flip (++)) bID [SimultAt Not_Waiting (Rel i) 
                                                                     [bID]]
     return bID 

convertAfter :: Interval -> P.EExpr -> BlockID -> Transformer ()
convertAfter i e b = 
  do n <- use $ currentRule 
     let bsn = "rule_" ++ (show n) ++ "_after" 
     let bsb = [SimultAt Not_Waiting (Rel i) [b]]
     bID <- addBlock bsn bsb 
     convertEExpr e bID 

convertInterrupt :: P.Extern -> BlockID -> Transformer ()
convertInterrupt (P.Extern s) b = addToEvent (Interrupt s) b 

-- This converts the shield for the cooldown block into 3 seperate blocks 
--  bs : The bootstrap block that initializes the variable
--  bc : The check, which will make sure enough time has passed since the 
--       last invocation of the command. 
--  bt : The Initializer which will make sure the variable we us is initialized
-- It would probably be easier to have this as a EventBased structure
-- and create a new BlockTransformer to convert it, but I can't be arsed to 
-- figure that out right now. 
convertCooldown :: P.EExpr -> Interval -> BlockID -> Transformer ()
convertCooldown e (Interval i) b = 
  do n <- use $ currentRule 
     vc <- getNextCounter 
     let vName = "_rule_" ++ (show n) ++ "_var_" ++ (show vc)
     let btName = "rule_" ++ (show n) ++ "_cooldown_if_true" 
     rt <- getNextReg -- next time
     btID <- addBlock btName [SimultAt Not_Waiting Now [b],
                              Call (Register rt) (ExternCall "Get_Time") [],
                              Store (VarID vName) (Reg rt)]
     let bcName = "rule_" ++ (show n) ++ "_cooldown_guard"
     rc <- getNextReg 
     rs <- getNextReg 
     rg <- getNextReg
     bcID <- addBlock bcName [Call (Register rc) (ExternCall "Get_Time") [],
                              BinaryOp (Register rs) 
                                       Subtract 
                                       (Reg rc)
                                       (Var (VarID vName)),
                              BinaryOp (Register rg) 
                                       Greater_Than 
                                       (Reg rs) 
                                       (Lit (Int i)),
                              If (Reg rg) (Just btID) (Nothing)]
     let bsName = "rule_" ++ (show n) ++ "_cooldown_init"
     bsID <- addBlock bsName [Store (VarID vName) (Lit (Int 0))]
     addToEvent Boot bsID
     convertEExpr e bcID 

convertBegins :: P.VExpr -> Interval -> BlockID -> Transformer ()
convertBegins v i b =
  do n <- use $ currentRule
     vc <- getNextCounter
     let vName = "_rule_" ++ (show n) ++ "_var_" ++ (show vc) 
     let bName = "rule_" ++ (show n) ++ "_begins_guard" 
     (bgID,bgVal) <- convertVExprToBlock bName v 
     ru <- getNextReg
     rc <- getNextReg
     ir.blocks %= Map.insertWith (flip (++)) bgID [UnaryOp (Register ru)
                                                    Logical_Not
                                                    (Var (VarID vName)),
                                            BinaryOp (Register rc) 
                                                     Logical_And
                                                     bgVal
                                                     (Reg ru),
                                            If (Reg rc) (Just b) (Nothing),
                                            Store (VarID vName) bgVal ]
     let bsName = "rule_" ++ (show n) ++ "_begins_init"
     bsID <- addBlock bsName [Store (VarID vName) (Lit (Bool False))]
     bID <- convertPeriodic "begins_bootstrap" i Now bgID
     addToEvent Boot bsID
     addToEvent Boot bID
     
 

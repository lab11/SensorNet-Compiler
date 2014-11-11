{-# LANGUAGE QuasiQuotes #-}

module Language.EventBased.ParserSpec (spec) where

import SpecHelper
import Data.String.Here (here,i)

import Language.EventBased.Parser
import Language.EventBased.Lexer (tokenize)

import Control.Exception (evaluate)
import Control.DeepSeq (force)

import Data.Time (ParseTime)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (DiffTime,secondsToDiffTime)
import Data.Time.Format (parseTimeOrError,defaultTimeLocale)
import Data.Time.LocalTime (LocalTime(..),TimeOfDay(..))



-- Helper Funtions to assemble time literals 

makeDateTime mm dd yy h m s = (LocalTime (fromGregorian yy mm dd)
                                                       (TimeOfDay h m s))
-- General utility functions

parseTest p s e = it ("parsing " ++ (show s)) $ do 
                      (p s) `shouldBe` e

parseError p s = it ("errors with " ++ (show s)) $ do 
                     ((evaluate . p)  s) `shouldThrow` anyErrorCall

runTests t = mapM_ t

-- Setup Testing for Value Expressions 

valExpr = valParse . tokenize

valTest = parseTest valExpr 
valParenTest s = parseTest valExpr ("(" ++ s ++ ")")

valTestSuite s a = do 
                      valTest s a
                      valParenTest s a 

valError = parseError valExpr

valTests = runTests (uncurry valTestSuite)

valErrors = runTests valError

valStrTest s = (show s, VEStr s)

--- Setup Testing for Action Expressions 

actExpr = actParse . tokenize

actTest = parseTest actExpr 

actError = parseError actExpr

actTests = runTests (uncurry actTest)

actErrors = runTests actError

--- Setup Testing for Event Expressions 

evtExpr = evtParse . tokenize

evtTest = parseTest evtExpr 

evtError = parseError evtExpr

evtTests = runTests (uncurry evtTest)

evtErrors = runTests evtError

-- Simple Static Tests for Literal Values 

valLiteralPairs = [

  -------- Integers ----------

  ("1",VEInt 1),       
  ("-1",VEInt (-1)),
  ("0",VEInt 0),

  -------- Strings ----------

  valStrTest "Initial String", 
  valStrTest "String with \n escapes",
  --valStrTest "String with an \" escaped quotation mark",

  -------- Floating Point Numbers ----------

  ("1.01",VEFlt 1.01),
  ("-234e4", VEFlt (-2340000.0)),
  ("5.64E-2", VEFlt 0.0564),

  -------- Booleans ---------
  
  ("True",VEBool True),
  ("False",VEBool False),

  ------- Identifiers -------
  
  ("local_variable",VEId "local_variable"),
  ("camelCase",VEId "camelCase"),
  ("numberedVar1",VEId "numberedVar1")]

-- Static Tests for Expressions

valSimpleExpressionPairs = [

  --------- Arithmetic Operators ----------

  ("1 + 2.3",VEBinop Add (VEInt 1) (VEFlt 2.3)),
  ("1 - 2.3",VEBinop Subtract (VEInt 1) (VEFlt 2.3)),
  ("1 * 2.3",VEBinop Multiply (VEInt 1) (VEFlt 2.3)),
  ("1 / 2.3",VEBinop Divide (VEInt 1) (VEFlt 2.3)),

  ---------- Boolean Ops --------------
  
  ("True && False",VEBinop Logical_And (VEBool True) (VEBool False)),
  ("True || False",VEBinop Logical_Or (VEBool True) (VEBool False)),
  ("True ^ False",VEBinop Logical_Xor (VEBool True) (VEBool False)),
  ("! True",VEUnop Logical_Not $ VEBool True),

  ---------- Comparison Ops ------------
  
  ("varA >= 10", VEBinop Greater_Than_Equals (VEId "varA") (VEInt 10)),
  ("varA > 10", VEBinop Greater_Than (VEId "varA") (VEInt 10)),
  ("varA <= 10", VEBinop Less_Than_Equals (VEId "varA") (VEInt 10)),
  ("varA < 10", VEBinop Less_Than (VEId "varA") (VEInt 10)),
  ("varA == 10", VEBinop Structural_Equality (VEId "varA") (VEInt 10)),

  ------------- String Ops --------------
  
  ([i|"this" << " and that."|], VEBinop String_Append (VEStr "this") 
                                                      (VEStr " and that."))

  ]
  
valPredAndAssocPairs = [

  -------- Associativity Tests -----------

  ("1 + 2 + 3", VEBinop Add (VEBinop Add (VEInt 1) (VEInt 2)) (VEInt 3)),
  ("1 - 2 + 3", VEBinop Add (VEBinop Subtract (VEInt 1) (VEInt 2)) (VEInt 3)),
  ("1 * 2 + 3", VEBinop Add (VEBinop Multiply (VEInt 1) (VEInt 2)) (VEInt 3)),

  ------------ Precedence Tests -------------

  ("1 + 2 * 3", VEBinop Add (VEInt 1) (VEBinop Multiply (VEInt 2) (VEInt 3))),
  ("(1 + 2) * 3", VEBinop Multiply (VEBinop Add (VEInt 1) (VEInt 2)) (VEInt 3)),
  ("! var1 && ! var2",VEBinop Logical_And (VEUnop Logical_Not (VEId "var1"))
                                          (VEUnop Logical_Not (VEId "var2")))
  ]

valNonAssocErrors = [
  "1 == 2 == 3",
  "1 >= 2 >= 3",
  "1 <= 2 <= 3",
  "1 < 2 < 3",
  "1 > 2 > 3",
  "1 > 2 < 3"
  ]

valCallExprPairs = [

  ("TestCall()",VECall "TestCall" []),
  ("TestCall(1,2,3,4)",VECall "TestCall" [VEInt 1,VEInt 2,VEInt 3, VEInt 4]),
  ("TestCall(1 + 2, Nested_Call())",VECall "TestCall" 
                                  [VEBinop Add (VEInt 1) (VEInt 2),
                                  VECall "Nested_Call" []])
  ]
  
-- Static Tests for Actions 

actGatherPairs = [
  ([here|
  GATHER { 
    SAVE Node_Id() AS id, 
    SAVE Temperature() AS temp, 
    SAVE Other_Sensor(1,2,3) AS other
  } INTO Table_Temp_Data;
  |], AEGather 
        [Record (VECall "Node_Id" []) "id",
        Record (VECall "Temperature" []) "temp",
        Record (VECall "Other_Sensor" [VEInt 1,VEInt 2,VEInt 3]) "other"]
      "Table_Temp_Data" )]

actSendPairs = [
  ([i|SEND foo@foobar.com "This is a string";|]
  ,AESend (Email "foo@foobar.com") (VEStr "This is a string"))]

actExecutePairs = [
  ([i|EXECUTE Get_Sample();|],AEExec $ VECall "Get_Sample" [])]

actIfPairs = [
  ([here|

  IF (2 < 3){
    SEND foo@foobar.com "This is a string";
    EXECUTE Get_Sample();
  };

  |], AEIf (VEBinop Less_Than (VEInt 2) (VEInt 3))
           [AESend (Email "foo@foobar.com") (VEStr "This is a string"),
            AEExec $ VECall "Get_Sample" []] [] ) ,
  ([here|

  IF(2 < 3) {
    SEND foo@foobar.com "This is a string";
  } ELSE { 
    EXECUTE Get_Sample();
  };

  |], AEIf (VEBinop Less_Than (VEInt 2) (VEInt 3))
           [AESend (Email "foo@foobar.com") (VEStr "This is a string")]
           [AEExec $ VECall "Get_Sample" []] )
  ]

actDoPairs = [
  ("DO this;", AEDo $ ID "this")]

actAssignPairs = [
  ("foo := bar + 1;", AEVassign (ID "foo") (VEBinop Add (VEId "bar") (VEInt 1)))
  ]

-- Event action tests 

evtEveryPairs = [
  ("EVERY 5 mins", EVEvery (Interval 300)),
  ("EVERY 10m 12s STARTING AT  12/31/2048 21:53:00",
        EVStartingAt (Interval 612) (makeDateTime 12 31 2048 21 53 0))]

evtAfterPairs = [
  ("3m AFTER EVERY 10m", EVAfter (Interval 180) (EVEvery (Interval 600)))]
  
evtInterruptPairs = [
  ("INTERRUPT Movement_Seen",EVInterrupt (Extern "Movement_Seen"))]

--  

spec :: Spec
spec =  do
  describe "Value Expressions" $ do
    context "Literals" $ do
      context "Simple Static Tests" $ do
        valTests valLiteralPairs
      it "Values that should not parse" $ 
        pendingWith "TODO: add tests for identifiers and call expressions"
      it "QuickCheck Based Tests" $
        pendingWith "TODO: Learn Quick Check"
    context "Simple Expressions" $ do
      valTests valSimpleExpressionPairs
    context "Precedence and Associativity" $ do
      valTests valPredAndAssocPairs
      valErrors valNonAssocErrors
    context "Call Expression Tests" $ do 
      valTests valCallExprPairs
    context "Arbitrary Value Composition Tests" $ do
      it "Generated Action Tests" $ do 
        pendingWith "TODO : Learn QuickCheck"
  describe "Action Expressions" $ do
    context "Gather Actions" $ do 
      actTests actGatherPairs 
      it "Values that should not parse" $ 
        pendingWith "We need counter examples" 
    context "Send Actions" $ do
      actTests actSendPairs
      it "Values that should not parse" $ 
        pendingWith "We need counter examples" 
    context "Execute Actions" $ do
      actTests actExecutePairs
      it "Values that should not parse" $ 
        pendingWith "We need counter examples" 
    context "If Actions" $ do 
      actTests actIfPairs
      it "Values that should not parse" $ 
        pendingWith "We need counter examples" 
    context "Do Actions" $ do 
      actTests actDoPairs
      it "Values that should not parse" $ 
        pendingWith "We need counter examples"
    context "Assignment Actions" $ do 
      actTests actAssignPairs 
      it "Values that should not parse" $ 
        pendingWith "We need counter examples"
    context "Action Blocks" $ do 
      it "Test a number of blocks" $
        pendingWith "TODO: Need to learn QuickCheck to do this properly"
  describe "Event Expressions" $ do 
    context "EVERY clauses" $ do 
      evtTests evtEveryPairs
    context "AFTER clauses" $ do 
      evtTests evtAfterPairs
    context "INTERRUPT clauses" $ do 
      evtTests evtInterruptPairs
    context "WITH COOLDOWN clases" $ do 
      it "write tests" $ do 
        pendingWith "write the tests" 
    context "AFTER BEGINS/ENDS clases" $ do 
      it "write tests" $ do 
        pendingWith "write the tests" 

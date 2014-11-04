{-# LANGUAGE QuasiQuotes #-}

module Language.EventBased.ParserSpec (spec) where

import SpecHelper
import Data.String.Here (here,i)
import Language.EventBased.Parser
import Language.EventBased.Lexer (tokenize)

-- General utility functions

parseTest p s e = it ("parsing " ++ (show s)) $ do 
                      (p s) `shouldBe` e

parseError p s = it ("parsing " ++ (show s)) $ 
                     (p s) `shouldThrow` anyErrorCall

runTests t = mapM_ $ uncurry t

-- Setup Testing for Value Expressions 

valExpr = valParse . tokenize

valTest = parseTest valExpr 
valParenTest s = parseTest valExpr ("(" ++ s ++ ")")

valTestSuite s a = do 
                      valTest s a
                      valParenTest s a 

-- valError = parseError valExpr

valTests = runTests valTestSuite

valStrTest s = (show s, VEStr s)

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

valSimpleExpressions = [

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
  


spec :: Spec
spec =  do
  describe "Value Expressions" $ do
    context "Literals" $ do
      context "Simple Static Tests" $ do
        valTests valLiteralPairs
      it "QuickCheck Based Tests" $
        pendingWith "TODO: Learn Quick Check"
      context "Simple Expressions" $ do
        valTests valSimpleExpressions


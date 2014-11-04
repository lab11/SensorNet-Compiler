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

-- valError = parseError valExpr

valTests = runTests valTest

valStrTest s = (show s, VEStr s)

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



spec :: Spec
spec =  do
  describe "Value Expressions" $ do
    context "Literals" $ do
      context "Simple Static Tests" $ do
        valTests valLiteralPairs
      it "QuickCheck Based Tests" $
        pendingWith "TODO: Learn Quick Check"
:

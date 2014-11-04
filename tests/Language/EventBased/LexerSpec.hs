module Language.EventBased.LexerSpec (spec) where

import SpecHelper
import Data.Char
import Data.Time.Calendar
import Data.Time.LocalTime


tokenTest s a = it ("should parse " ++ (show s)) $ do
                  tokenize s `shouldBe` a

tokenError s a = it ("should error " ++ (show s)) $ do
                      tokenize s `shouldSatisfy` (/= a)

-- Create a series of tests that make sure whether a particular token 
-- is parsed correctly

tokenSpace s a =  do
                    tokenTest ("  " ++ s) a
                    tokenTest (s ++ " ") a 
                    tokenTest (s ++ "\t ") a
                    tokenTest (" \t " ++ s ++ "\t ") a
                    tokenTest (" \n " ++ s ++ "\n") a

tokenFullTest s a = do
                      tokenTest s [a]
                      tokenSpace s [a] 

-- Special Tests that are useful for keywords 

keywordFullTest s a = do 
                        tokenTest s [a]
                        tokenError (map toLower s) [a] 
                        tokenSpace s [a]

-- Given a list of tests for a particular element do all of them

keywordTests = mapM_ $ uncurry keywordFullTest

tokenTests = mapM_ $ uncurry tokenFullTest

-- Helper Funtions to assemble time literals 

makeTimeOfDay h m s = Lit $ DailyTime $ TimeOfDay h m s 

makeDateTime mm dd yy h m s = Lit (AbsTime (LocalTime (fromGregorian yy mm dd)
                                                       (TimeOfDay h m s)))
  
-- Turn a list of inputs that should parse to the same token into a set 
-- of (string,token) pairs 

seperateTests = ( >>= (\ (l,a) -> map (\ s -> (s,a)) l) )

-- Saved Variables that hold keyword token pairs so that we can generate a 
-- number of tests as needed 

keywordPairs = [("ON",Key On), 
                ("EVERY",Key Every),
                ("AFTER",Key After),
                ("BEGINS",Key Begins),
                ("END",Key End),
                ("PERFORM",Key Perform), 
                ("WITH COOLDOWN",Key With_Cooldown),
                ("WITHIN",Key Within),
                ("INTERRUPT",Key Interrupt),
                ("GATHER",Key Gather),
                ("SEND",Key Send),
                ("EXECUTE",Key Execute),
                ("IF",Key If),
                ("DO",Key Do),
                ("SAVE",Key Save),
                ("INTO",Key Into),
                ("AS",Key As),
                ("SET OPTIONS",Key Set_Options),
                ("STARTING AT",Key Starting_At),
                ("UPDATE",Key Update)]

timeKeyPairs = seperateTests 
                [(["d","day","days"],RelTime Days),
                 (["h","hr","hrs","hour","hours"],RelTime Hours),
                 (["m","mins","min","minute","minutes"],RelTime Minutes),
                 (["s","sec","secs","second","seconds"],RelTime Seconds)]

absoluteTimePairs = [("10/10/1923",makeDateTime 10 10 1923 0 0 0),
                     ("12/31/2048 04:53:00 am",makeDateTime 12 31 2048 4 53 0),
                     ("12/31/2048 04:53:00 pm",makeDateTime 12 31 2048 16 53 0),
                     ("12/31/2048 04:53:00",makeDateTime 12 31 2048 4 53 0),
                     ("12/31/2048 21:53:00",makeDateTime 12 31 2048 21 53 0),
                     ("23:23:15",makeTimeOfDay 23 23 15),
                     ("11:23:15 pm",makeTimeOfDay 23 23 15)]

flowControlPairs = [("(",Flow OpParen),
                    (")",Flow ClParen),
                    ("{",Flow OpBracket),
                    ("}",Flow ClBracket),
                    ("[",Flow OpSqBracket),
                    ("]",Flow ClSqBracket),
                    (";",Flow Semicolon),
                    (":",Flow Colon),
                    (",",Flow Comma),
                    (":=",Flow Assign)]

operatorPairs = [("&&",Op Logical_And),
                 ("||",Op Logical_Or),
                 ("^",Op Logical_Xor),
                 ("!",Op Logical_Not),
                 ("=",Op Structural_Equality),
                 (">",Op Greater_Than),
                 (">=",Op Greater_Than_Equals),
                 ("<",Op Less_Than),
                 ("<=",Op Less_Than_Equals),
                 ("<<",Op String_Append), 
                 ("+",Op Add), 
                 ("-",Op Subtract),
                 ("*",Op Multiply), 
                 ("/",Op Divide)]

spec :: Spec
spec = do
  describe "Keywords" $ 
    keywordTests keywordPairs
  describe "Times" $ do
    context "Parses of time keywords" $ do
      tokenTests timeKeyPairs
    context "Parses of absolute times" $ do
      tokenTests absoluteTimePairs
  describe "Flow control elements" $ do
    tokenTests flowControlPairs
  describe "Operators" $ do
    tokenTests operatorPairs
  describe "Other Literals" $ do
    it "TODO: will run quickcheck tests for these" $
      pendingWith "Need to learn QuickCheck to do this properly"
  describe "Integration Tests" $ 
    it "TODO: will run quickcheck tests for these" $
      pendingWith "Need to learn QuickCheck to do this properly"
  describe "Error Tests" $  
    it "TODO: will run quickcheck tests for these" $
      pendingWith "Need to learn QuickCheck to do this properly"

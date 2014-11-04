module Language.EventBased.LexerSpec (spec) where

import SpecHelper
import Data.Char

a <==> b = a `shouldBe` b

tokenTest s a = it ("should parse \"" ++ s ++ "\"") $ do
                  tokenize s <==> a

tokenError s a = it ("should error \"" ++ s ++ "\"") $ do
                      tokenize s `shouldSatisfy` (/= a)

keywordError s = tokenError $ map toLower s 

tokenSpace s a =  do
                    tokenTest ("  " ++ s) a
                    tokenTest (s ++ " ") a 
                    tokenTest (s ++ "\t ") a

keywordFullTest s a = tokenTest s [a] >> keywordError s [a] >> tokenSpace s [a]

tokenFullTest s a = tokenTest s [a] >> tokenSpace s [a] 

keywordTests = mapM_ $ uncurry keywordFullTest

tokenTests = mapM_ $ uncurry tokenFullTest

tokenVarTests l a = tokenTests $ map (\ s -> (s,a)) l

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

spec :: Spec
spec = do
  describe "Keywords" $ 
    keywordTests keywordPairs
  describe "Times" $ do
    context "Parses of time keywords" $ do
      tokenVarTests ["d","day","days"] (RelTime Days)
      tokenVarTests ["h","hr","hrs","hour","hours"] (RelTime Hours)
      tokenVarTests ["m","mins","min","minute","minutes"] (RelTime Minutes)
      tokenVarTests ["s","sec","secs","second","seconds"] (RelTime Seconds)
    -- context "Parses of absolute times" $ do
      

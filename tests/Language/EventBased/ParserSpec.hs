{-# LANGUAGE QuasiQuotes #-}

module Language.EventBased.ParserSpec (spec) where

import SpecHelper
import Data.String.Here (here,i)



spec :: Spec
spec =  do
  describe "Placeholder" $ do
    it "Placeholder" $
      pendingWith "Placeholder"


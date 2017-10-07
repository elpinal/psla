module LibSpec where

import Test.Hspec

import Control.Monad.State.Lazy

import Lib

spec :: Spec
spec = do
  describe "installFlags" $
    it "parses one or no flag in args" $ do
      let xs = ["-config", "conf", "-framework", "arg1", "arg2"]
      let (flag, args) = runState installFlags xs
      flag `shouldBe` Just (Config "conf")
      args `shouldBe` ["-framework", "arg1", "arg2"]

  describe "parseFlag" $ do
    it "returns arguments as it is if no flag are given" $ do
      let xs = ["arg"]
      let (flag, args) = runState (parseFlag installFlags) xs
      flag `shouldBe` []
      args `shouldBe` xs

      let (flag, args) = runState (parseFlag installFlags) []
      flag `shouldBe` []
      args `shouldBe` []

    it "parses flags in args" $ do
      let xs = ["-config", "conf", "-framework", "arg1", "arg2"]
      let (flag, args) = runState (parseFlag installFlags) xs
      flag `shouldBe` [Config "conf", Framework]
      args `shouldBe` ["arg1", "arg2"]

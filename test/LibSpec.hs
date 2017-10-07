module LibSpec where

import Test.Hspec

import Control.Monad.State.Lazy

import Lib

spec :: Spec
spec =
  describe "installFlags" $
    it "parses one or no flag in args" $ do
      let xs = ["-config", "conf", "-framework", "arg1", "arg2"]
      let (flag, args) = runState installFlags xs
      flag `shouldBe` Just (Config "conf")
      args `shouldBe` ["-framework", "arg1", "arg2"]

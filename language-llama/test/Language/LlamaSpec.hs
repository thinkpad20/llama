module Language.LlamaSpec (main, spec) where

import Test.Hspec
import Control.Monad.Trans (liftIO)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "some blibberblobbishness" $ do
    it "should work fine" $ do
      True `shouldBe` False
    it "really should work fine" $ do
      True `shouldBe` True

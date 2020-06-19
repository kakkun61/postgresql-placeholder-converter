{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.Placeholder.ConvertSpec (spec) where

import Database.PostgreSQL.Placeholder.Convert

import Test.Hspec

spec :: Spec
spec = do
  describe "convertQuestionMarkStyleToDollarSignStyle" $ do
    it "SELECT * FROM person" $ do
      convertQuestionMarkStyleToDollarSignStyle "SELECT * FROM person" `shouldBe` Right "SELECT * FROM person"

    it "SELECT * FROM person WHERE id = ?" $ do
      convertQuestionMarkStyleToDollarSignStyle "SELECT * FROM person WHERE id = ?" `shouldBe` Right "SELECT * FROM person WHERE id = $1"

    it "SELECT * FROM person WHERE name = '?'" $ do
      convertQuestionMarkStyleToDollarSignStyle "SELECT * FROM person WHERE name = '?'" `shouldBe` Right "SELECT * FROM person WHERE name = '?'"

    it "SELECT * FROM person WHERE name = $$?$$" $ do
      convertQuestionMarkStyleToDollarSignStyle "SELECT * FROM person WHERE name = $$?$$" `shouldBe` Right "SELECT * FROM person WHERE name = $$?$$"

    it "SELECT * FROM person WHERE name = $name$?$name$" $ do
      convertQuestionMarkStyleToDollarSignStyle "SELECT * FROM person WHERE name = $name$?$name$" `shouldBe` Right "SELECT * FROM person WHERE name = $name$?$name$"

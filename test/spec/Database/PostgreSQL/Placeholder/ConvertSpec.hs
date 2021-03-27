{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.Placeholder.ConvertSpec (spec) where

import Database.PostgreSQL.Placeholder.Convert

import Test.Hspec

spec :: Spec
spec = do
  describe "convertQuestionMarkStyleToDollarSignStyle" $ do
    it "SELECT * FROM person" $ do
      convertQuestionMarkStyleToDollarSignStyle "SELECT * FROM person" `shouldReturn` "SELECT * FROM person"

    it "SELECT * FROM person WHERE id = ?" $ do
      convertQuestionMarkStyleToDollarSignStyle "SELECT * FROM person WHERE id = ?" `shouldReturn` "SELECT * FROM person WHERE id = $1"

    it "SELECT * FROM person WHERE name = '?'" $ do
      convertQuestionMarkStyleToDollarSignStyle "SELECT * FROM person WHERE name = '?'" `shouldReturn` "SELECT * FROM person WHERE name = '?'"

    it "SELECT * FROM person WHERE name = $$?$$" $ do
      convertQuestionMarkStyleToDollarSignStyle "SELECT * FROM person WHERE name = $$?$$" `shouldReturn` "SELECT * FROM person WHERE name = $$?$$"

    it "SELECT * FROM person WHERE name = $name$?$name$" $ do
      convertQuestionMarkStyleToDollarSignStyle "SELECT * FROM person WHERE name = $name$?$name$" `shouldReturn` "SELECT * FROM person WHERE name = $name$?$name$"

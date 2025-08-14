module Main (main) where

import Test.Hspec
import Web.Apiframe.Types
import Data.Aeson (decode, encode)

main :: IO ()
main = hspec $ do
  describe "Web.Apiframe.Types" $ do
    describe "JSON encoding/decoding" $ do
      it "encodes and decodes AspectRatio correctly" $ do
        let ar = AspectRatio16x9
        decode (encode ar) `shouldBe` Just ar
      
      it "encodes and decodes ProcessMode correctly" $ do
        let pm = ProcessModeTurbo
        decode (encode pm) `shouldBe` Just pm
      
      it "encodes and decodes Direction correctly" $ do
        let dir = DirectionUp
        decode (encode dir) `shouldBe` Just dir
      
      it "encodes ImagineRequest correctly" $ do
        let req = ImagineRequest
              { imaginePrompt = "test prompt"
              , imagineAspectRatio = Just AspectRatio1x1
              , imagineProcessMode = Just ProcessModeFast
              , imagineWebhookUrl = Nothing
              , imagineWebhookSecret = Nothing
              }
        let encoded = encode req
        encoded `shouldSatisfy` (/= "null")
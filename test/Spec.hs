{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Web.Apiframe.Types
import Data.Aeson (decode, encode, ToJSON, FromJSON)
import qualified Data.Text as T

-- Arbitrary instances for property testing

instance Arbitrary AspectRatio where
  arbitrary = elements [minBound..]

instance Arbitrary ProcessMode where
  arbitrary = elements [minBound..]

instance Arbitrary Direction where
  arbitrary = elements [minBound..]

instance Arbitrary UpscaleType where
  arbitrary = elements [minBound..]

instance Arbitrary UpscaleAltType where
  arbitrary = elements [minBound..]

instance Arbitrary ImageIndex where
  arbitrary = elements [minBound..]

instance Arbitrary Dimension where
  arbitrary = elements [minBound..]

instance Arbitrary TaskStatus where
  arbitrary = elements [minBound..]

instance Arbitrary ApiError where
  arbitrary = do
    msg <- T.pack <$> arbitrary
    return ApiError { apiErrorMsg = msg }

-- Property: JSON round-trip should preserve data
jsonRoundTrip :: (Eq a, ToJSON a, FromJSON a) => a -> Bool
jsonRoundTrip x = decode (encode x) == Just x

main :: IO ()
main = hspec $ do
  describe "Web.Apiframe.Types" $ do
    describe "JSON encoding/decoding properties" $ do
      it "AspectRatio JSON round-trip property" $
        property (jsonRoundTrip :: AspectRatio -> Bool)
      
      it "ProcessMode JSON round-trip property" $
        property (jsonRoundTrip :: ProcessMode -> Bool)
      
      it "Direction JSON round-trip property" $
        property (jsonRoundTrip :: Direction -> Bool)
      
      it "UpscaleType JSON round-trip property" $
        property (jsonRoundTrip :: UpscaleType -> Bool)
      
      it "UpscaleAltType JSON round-trip property" $
        property (jsonRoundTrip :: UpscaleAltType -> Bool)
      
      it "ImageIndex JSON round-trip property" $
        property (jsonRoundTrip :: ImageIndex -> Bool)
      
      it "Dimension JSON round-trip property" $
        property (jsonRoundTrip :: Dimension -> Bool)
      
      it "TaskStatus JSON round-trip property" $
        property (jsonRoundTrip :: TaskStatus -> Bool)
      
      it "ApiError JSON round-trip property" $
        property (jsonRoundTrip :: ApiError -> Bool)
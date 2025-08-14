{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Test.Hspec
import Test.QuickCheck
import Web.Apiframe.Types
import Web.Apiframe.Client (mkApiframeClient, imagine, fetch)
import Data.Aeson (decode, encode, ToJSON, FromJSON, Value(..))
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
import qualified Data.Vector as V
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)

instance Arbitrary AspectRatio where
  arbitrary = do
    width <- choose (1, 32)  -- Common aspect ratio range
    height <- choose (1, 32)
    return $ AspectRatio width height

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

-- Helper function to download image from URL
downloadImage :: String -> FilePath -> IO ()
downloadImage url filePath = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest url
  response <- httpLbs request manager
  LBS.writeFile filePath (responseBody response)

-- Extract image URL from task result
extractImageUrl :: Value -> Maybe String
extractImageUrl (Object obj) = case KM.lookup (Key.fromString "images") obj of
  Just (Array arr) -> case V.toList arr of
    [String url] -> Just (T.unpack url)
    _ -> Nothing
  _ -> Nothing
extractImageUrl _ = Nothing

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

  -- Integration test (only runs if API key is set)
  describe "Web.Apiframe.Client Integration" $ do
    it "README.md example - generate image and save to disk" $ do
      apiKeyMaybe <- lookupEnv "APIFRAME_API_KEY"
      case apiKeyMaybe of
        Nothing -> pendingWith "APIFRAME_API_KEY not set - skipping integration test"
        Just apiKeyStr -> do
          -- This matches the README.md example exactly
          let apiKey = T.pack apiKeyStr
          client <- mkApiframeClient apiKey
          
          -- Generate an image (matching README example)
          let request = ImagineRequest
                { imaginePrompt = "a nice day in the desert with my dog"
                , imagineAspectRatio = Just (AspectRatio 3 2)  -- 3:2 ratio
                , imagineProcessMode = Just ProcessModeFast
                , imagineWebhookUrl = Nothing
                , imagineWebhookSecret = Nothing
                }
          
          result <- imagine client request
          case result of
            Left err -> expectationFailure $ "API call failed: " ++ show err
            Right TaskResponse{..} -> do
              -- Task was submitted successfully
              taskId `shouldSatisfy` (not . T.null)
              
              -- Wait for task to complete and fetch result
              let fetchReq = FetchRequest { fetchTaskId = taskId }
              
              -- Poll for completion (simple retry logic)
              let pollForCompletion retries = do
                    if retries <= 0
                      then expectationFailure "Task did not complete in time"
                      else do
                        fetchResult <- fetch client fetchReq
                        case fetchResult of
                          Left err -> expectationFailure $ "Fetch failed: " ++ show err
                          Right FetchResponse{fetchStatus=status, fetchResult=maybeResult} -> case status of
                            StatusCompleted -> case maybeResult of
                              Just taskResult -> case extractImageUrl taskResult of
                                Just imageUrl -> do
                                  -- Create output directory
                                  createDirectoryIfMissing True "test-output"
                                  let filename = "test-output/readme-example-" ++ T.unpack taskId ++ ".jpg"
                                  
                                  -- Download and save the image
                                  downloadImage imageUrl filename
                                  putStrLn $ "✅ Image saved to: " ++ filename
                                  
                                Nothing -> expectationFailure "No image URL found in result"
                              Nothing -> expectationFailure "No result data returned"
                            StatusFailed -> expectationFailure "Task failed"
                            StatusPending -> do
                              putStrLn "⏳ Task pending, waiting..."
                              -- Wait 5 seconds before retry (in real app, use proper delay)
                              pollForCompletion (retries - 1)
                            StatusProcessing -> do
                              putStrLn "🔄 Task processing, waiting..."
                              pollForCompletion (retries - 1)
              
              pollForCompletion (12 :: Int)  -- Try for ~1 minute with 5s intervals

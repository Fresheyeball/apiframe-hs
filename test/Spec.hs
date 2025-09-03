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

-- Arbitrary instances for newtypes
instance Arbitrary TaskId where
  arbitrary = TaskId . T.pack <$> arbitrary

instance Arbitrary Prompt where
  arbitrary = Prompt . T.pack <$> arbitrary

instance Arbitrary ImageUrl where
  arbitrary = ImageUrl . T.pack <$> arbitrary

instance Arbitrary Base64Image where
  arbitrary = Base64Image . T.pack <$> arbitrary

instance Arbitrary VariationsIndex where
  arbitrary = VariationsIndex . T.pack <$> arbitrary

instance Arbitrary WebhookUrl where
  arbitrary = WebhookUrl . T.pack <$> arbitrary

instance Arbitrary WebhookSecret where
  arbitrary = WebhookSecret . T.pack <$> arbitrary

instance Arbitrary Email where
  arbitrary = Email . T.pack <$> arbitrary

instance Arbitrary Plan where
  arbitrary = Plan . T.pack <$> arbitrary

instance Arbitrary ErrorMessage where
  arbitrary = ErrorMessage . T.pack <$> arbitrary

instance Arbitrary PngUrl where
  arbitrary = PngUrl . T.pack <$> arbitrary

instance Arbitrary Mp4Url where
  arbitrary = Mp4Url . T.pack <$> arbitrary

instance Arbitrary Percentage where
  arbitrary = Percentage <$> choose (0, 100)

instance Arbitrary Seed where
  arbitrary = Seed . T.pack <$> arbitrary

instance Arbitrary Sref where
  arbitrary = Sref . T.pack <$> arbitrary

instance Arbitrary ApiError where
  arbitrary = do
    msg <- T.pack <$> arbitrary
    return ApiError { apiErrorMsg = ErrorMessage msg }

instance Arbitrary TaskType where
  arbitrary = oneof
    [ pure TaskTypeImagine
    , pure TaskTypeImagineVideo
    , pure TaskTypeReroll
    , TaskTypeUpscale <$> arbitrary
    , pure TaskTypeUpscaleCreative
    , pure TaskTypeUpscaleSubtle
    , pure TaskTypeUpscale2x
    , pure TaskTypeUpscale4x
    , TaskTypeVariation <$> arbitrary
    , pure TaskTypeVariationStrong
    , pure TaskTypeVariationSubtle
    , pure TaskTypeFaceswap
    , pure TaskTypeInpaint
    , TaskTypeOutpaint <$> choose (1, 4)
    , TaskTypePan <$> arbitrary
    , pure TaskTypeShorten
    , pure TaskTypeDescribe
    , pure TaskTypeBlend
    , pure TaskTypeSeed
    ]

-- Arbitrary instances for FetchResponse record types
instance Arbitrary FetchProcessing where
  arbitrary = FetchProcessing <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary FetchImagineComplete where
  arbitrary = FetchImagineComplete <$> arbitrary <*> pure TaskTypeImagine <*> arbitrary <*> arbitrary <*> listOf arbitrary

instance Arbitrary FetchImagineVideoComplete where
  arbitrary = FetchImagineVideoComplete <$> arbitrary <*> pure TaskTypeImagineVideo <*> listOf arbitrary

instance Arbitrary FetchRerollComplete where
  arbitrary = FetchRerollComplete <$> arbitrary <*> pure TaskTypeReroll <*> arbitrary <*> listOf arbitrary

instance Arbitrary FetchUpscaleComplete where
  arbitrary = FetchUpscaleComplete <$> arbitrary <*> (TaskTypeUpscale <$> arbitrary) <*> arbitrary

instance Arbitrary FetchVariationComplete where
  arbitrary = FetchVariationComplete <$> arbitrary <*> (TaskTypeVariation <$> arbitrary) <*> arbitrary <*> listOf arbitrary

instance Arbitrary FetchFaceswapComplete where
  arbitrary = FetchFaceswapComplete <$> arbitrary <*> pure TaskTypeFaceswap <*> arbitrary

instance Arbitrary FetchInpaintComplete where
  arbitrary = FetchInpaintComplete <$> arbitrary <*> pure TaskTypeInpaint <*> arbitrary <*> listOf arbitrary

instance Arbitrary FetchOutpaintComplete where
  arbitrary = FetchOutpaintComplete <$> arbitrary <*> (TaskTypeOutpaint <$> choose (1, 4)) <*> arbitrary <*> listOf arbitrary

instance Arbitrary FetchPanComplete where
  arbitrary = FetchPanComplete <$> arbitrary <*> (TaskTypePan <$> arbitrary) <*> arbitrary <*> listOf arbitrary

instance Arbitrary FetchShortenComplete where
  arbitrary = FetchShortenComplete <$> arbitrary <*> pure TaskTypeShorten <*> listOf (T.pack <$> arbitrary) <*> (fmap (T.pack) <$> arbitrary)

instance Arbitrary FetchDescribeComplete where
  arbitrary = FetchDescribeComplete <$> arbitrary <*> pure TaskTypeDescribe <*> arbitrary <*> listOf (T.pack <$> arbitrary)

instance Arbitrary FetchBlendComplete where
  arbitrary = FetchBlendComplete <$> arbitrary <*> pure TaskTypeBlend <*> arbitrary <*> listOf arbitrary

instance Arbitrary FetchSeedComplete where
  arbitrary = FetchSeedComplete <$> arbitrary <*> pure TaskTypeSeed <*> arbitrary

instance Arbitrary FetchResponse where
  arbitrary = oneof
    [ FetchResponseProcessing <$> arbitrary
    , FetchResponseImagineComplete <$> arbitrary
    , FetchResponseImagineVideoComplete <$> arbitrary
    , FetchResponseRerollComplete <$> arbitrary
    , FetchResponseUpscaleComplete <$> arbitrary
    , FetchResponseVariationComplete <$> arbitrary
    , FetchResponseFaceswapComplete <$> arbitrary
    , FetchResponseInpaintComplete <$> arbitrary
    , FetchResponseOutpaintComplete <$> arbitrary
    , FetchResponsePanComplete <$> arbitrary
    , FetchResponseShortenComplete <$> arbitrary
    , FetchResponseDescribeComplete <$> arbitrary
    , FetchResponseBlendComplete <$> arbitrary
    , FetchResponseSeedComplete <$> arbitrary
    ]

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

      -- Newtype JSON round-trip tests
      it "TaskId JSON round-trip property" $
        property (jsonRoundTrip :: TaskId -> Bool)

      it "Prompt JSON round-trip property" $
        property (jsonRoundTrip :: Prompt -> Bool)

      it "ImageUrl JSON round-trip property" $
        property (jsonRoundTrip :: ImageUrl -> Bool)

      it "Base64Image JSON round-trip property" $
        property (jsonRoundTrip :: Base64Image -> Bool)

      it "VariationsIndex JSON round-trip property" $
        property (jsonRoundTrip :: VariationsIndex -> Bool)

      it "WebhookUrl JSON round-trip property" $
        property (jsonRoundTrip :: WebhookUrl -> Bool)

      it "WebhookSecret JSON round-trip property" $
        property (jsonRoundTrip :: WebhookSecret -> Bool)

      it "Email JSON round-trip property" $
        property (jsonRoundTrip :: Email -> Bool)

      it "Plan JSON round-trip property" $
        property (jsonRoundTrip :: Plan -> Bool)

      it "ErrorMessage JSON round-trip property" $
        property (jsonRoundTrip :: ErrorMessage -> Bool)

      it "PngUrl JSON round-trip property" $
        property (jsonRoundTrip :: PngUrl -> Bool)

      it "Mp4Url JSON round-trip property" $
        property (jsonRoundTrip :: Mp4Url -> Bool)

      it "Percentage JSON round-trip property" $
        property (jsonRoundTrip :: Percentage -> Bool)

      it "Seed JSON round-trip property" $
        property (jsonRoundTrip :: Seed -> Bool)

      it "Sref JSON round-trip property" $
        property (jsonRoundTrip :: Sref -> Bool)

      it "TaskType JSON round-trip property" $
        property (jsonRoundTrip :: TaskType -> Bool)

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
                { imaginePrompt = Prompt "a nice day in the desert with my dog"
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
              taskId `shouldSatisfy` (not . T.null . unTaskId)

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
                          Right response -> case response of
                            FetchResponseProcessing processing -> do
                              case fetchProcessingStatus processing of
                                StatusProcessing -> putStrLn $ "🔄 Task processing" ++ maybe "" (\p -> " (" ++ show (unPercentage p) ++ "%)") (fetchProcessingPercentage processing)
                                StatusStarting -> putStrLn "🚀 Task starting..."
                                _ -> putStrLn $ "⏳ Task status: " ++ show (fetchProcessingStatus processing)
                              pollForCompletion (retries - 1)
                            
                            FetchResponseImagineComplete imagineResult -> do
                              let imageUrls = fetchImagineImageUrls imagineResult
                              if null imageUrls
                                then expectationFailure "No image URLs returned"
                                else do
                                  -- Create output directory
                                  createDirectoryIfMissing True "test-output"
                                  let firstImageUrl = T.unpack $ unPngUrl $ head imageUrls
                                  let filename = "test-output/readme-example-" ++ T.unpack (unTaskId taskId) ++ ".png"
                                  
                                  -- Download and save the image
                                  downloadImage firstImageUrl filename
                                  putStrLn $ "✅ Image saved to: " ++ filename
                            
                            _ -> expectationFailure $ "Unexpected response type: " ++ show response

              pollForCompletion (12 :: Int)  -- Try for ~1 minute with 5s intervals

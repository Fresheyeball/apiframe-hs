{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE DerivingVia #-}
module Web.Apiframe.Types where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Data.Scientific
import Text.Read (readMaybe)
import Web.HttpApiData (ToHttpApiData, FromHttpApiData)

stripQuotes :: Text -> Text
stripQuotes = T.replace "\"" ""

newtype NoQuotes = NoQuotes Text
instance Show NoQuotes where
  show (NoQuotes t) = T.unpack . stripQuotes . T.pack $ show t
instance Read NoQuotes where
  readsPrec _ s =
    let trimmed = dropWhile (== ' ') s
    in case trimmed of
      ('"':_) ->
        -- Handle quoted string
        case reads trimmed :: [(String, String)] of
          [(str, remainder)] -> [(NoQuotes (T.pack str), remainder)]
          _ -> []
      _ ->
        -- Handle unquoted string - read until whitespace or end
        let (word, remainder) = break (`elem` (" \t\n"::String)) trimmed
        in ([(NoQuotes (T.pack word), remainder) | not (null word)])

newtype TaskId = TaskId Text
  deriving stock (Generic)
  deriving newtype (Eq, ToJSON, FromJSON)
  deriving (Show, Read) via NoQuotes

newtype Prompt = Prompt Text
  deriving stock (Generic)
  deriving newtype (Eq, ToJSON, FromJSON)
  deriving (Show, Read) via NoQuotes

newtype ImageUrl = ImageUrl Text
  deriving stock (Generic)
  deriving newtype (Eq, ToJSON, FromJSON)
  deriving (Show, Read) via NoQuotes

newtype Base64Image = Base64Image Text
  deriving stock (Generic)
  deriving newtype (Eq, ToJSON, FromJSON)
  deriving (Show, Read) via NoQuotes

newtype VariationsIndex = VariationsIndex Text
  deriving stock (Generic)
  deriving newtype (Eq, ToJSON, FromJSON)
  deriving (Show, Read) via NoQuotes

newtype WebhookUrl = WebhookUrl Text
  deriving stock (Generic)
  deriving newtype (Eq, ToJSON, FromJSON)
  deriving (Show, Read) via NoQuotes

newtype WebhookSecret = WebhookSecret Text
  deriving stock (Generic)
  deriving newtype (Eq, ToJSON, FromJSON)
  deriving (Show, Read) via NoQuotes

newtype Email = Email Text
  deriving stock (Generic)
  deriving newtype (Eq, ToJSON, FromJSON)
  deriving (Show, Read) via NoQuotes

newtype Plan = Plan Text
  deriving stock (Generic)
  deriving newtype (Eq, ToJSON, FromJSON)
  deriving (Show, Read) via NoQuotes

newtype ErrorMessage = ErrorMessage Text
  deriving stock (Generic)
  deriving newtype (Eq, ToJSON, FromJSON)
  deriving (Show, Read) via NoQuotes

newtype ApiKey = ApiKey Text
  deriving stock (Generic)
  deriving newtype (Eq, ToHttpApiData, FromHttpApiData)
  deriving (Show, Read) via NoQuotes

newtype PngUrl = PngUrl Text
  deriving stock (Generic)
  deriving newtype (Eq, ToJSON, FromJSON)
  deriving (Show, Read) via NoQuotes

newtype Mp4Url = Mp4Url Text
  deriving stock (Generic)
  deriving newtype (Eq, ToJSON, FromJSON)
  deriving (Show, Read) via NoQuotes

newtype Percentage = Percentage Int
  deriving stock (Generic)
  deriving newtype (Eq)
  deriving (Show, Read)

instance ToJSON Percentage where
  toJSON = toJSON . show . unPercentage

instance FromJSON Percentage where
  parseJSON z = case z of
    String x -> case readMaybe $ T.unpack x of
                  Nothing -> fail $ T.unpack x <> " not a number"
                  Just y -> return $ Percentage y
    Number x -> case toBoundedInteger x of
                  Nothing -> fail $ show x <> " is not an integer"
                  Just y -> return $ Percentage y
    _ -> fail $ show z <> " is not a percentage"

newtype Seed = Seed Text
  deriving stock (Generic)
  deriving newtype (Eq, ToJSON, FromJSON)
  deriving (Show, Read) via NoQuotes

newtype Sref = Sref Text
  deriving stock (Generic)
  deriving newtype (Eq, ToJSON, FromJSON)
  deriving (Show, Read) via NoQuotes

-- Helper functions to extract underlying text values
unTaskId :: TaskId -> Text
unTaskId (TaskId t) = t

unPrompt :: Prompt -> Text
unPrompt (Prompt t) = t

unImageUrl :: ImageUrl -> Text
unImageUrl (ImageUrl t) = t

unBase64Image :: Base64Image -> Text
unBase64Image (Base64Image t) = t

unVariationsIndex :: VariationsIndex -> Text
unVariationsIndex (VariationsIndex t) = t

unWebhookUrl :: WebhookUrl -> Text
unWebhookUrl (WebhookUrl t) = t

unWebhookSecret :: WebhookSecret -> Text
unWebhookSecret (WebhookSecret t) = t

unEmail :: Email -> Text
unEmail (Email t) = t

unPlan :: Plan -> Text
unPlan (Plan t) = t

unErrorMessage :: ErrorMessage -> Text
unErrorMessage (ErrorMessage t) = t

unApiKey :: ApiKey -> Text
unApiKey (ApiKey t) = t

unPngUrl :: PngUrl -> Text
unPngUrl (PngUrl t) = t

unMp4Url :: Mp4Url -> Text
unMp4Url (Mp4Url t) = t

unPercentage :: Percentage -> Int
unPercentage (Percentage i) = i

unSeed :: Seed -> Text
unSeed (Seed t) = t

unSref :: Sref -> Text
unSref (Sref t) = t

-- Request Types

data ImagineRequest = ImagineRequest
  { imaginePrompt :: Prompt
  , imagineAspectRatio :: Maybe AspectRatio
  , imagineProcessMode :: Maybe ProcessMode
  , imagineWebhookUrl :: Maybe WebhookUrl
  , imagineWebhookSecret :: Maybe WebhookSecret
  } deriving (Show, Eq, Generic)

instance ToJSON ImagineRequest where
  toJSON ImagineRequest{..} = object
    [ "prompt" .= imaginePrompt
    , "aspect_ratio" .= imagineAspectRatio
    , "process_mode" .= imagineProcessMode
    , "webhook_url" .= imagineWebhookUrl
    , "webhook_secret" .= imagineWebhookSecret
    ]

data UpscaleRequest = UpscaleRequest
  { upscaleParentTaskId :: TaskId
  , upscaleIndex :: ImageIndex
  , upscaleWebhookUrl :: Maybe WebhookUrl
  , upscaleWebhookSecret :: Maybe WebhookSecret
  } deriving (Show, Eq, Generic)

instance ToJSON UpscaleRequest where
  toJSON UpscaleRequest{..} = object
    [ "parent_task_id" .= upscaleParentTaskId
    , "index" .= upscaleIndex
    , "webhook_url" .= upscaleWebhookUrl
    , "webhook_secret" .= upscaleWebhookSecret
    ]

data UpscaleAltRequest = UpscaleAltRequest
  { upscaleAltParentTaskId :: TaskId
  , upscaleAltType :: UpscaleAltType
  , upscaleAltWebhookUrl :: Maybe WebhookUrl
  , upscaleAltWebhookSecret :: Maybe WebhookSecret
  } deriving (Show, Eq, Generic)

instance ToJSON UpscaleAltRequest where
  toJSON UpscaleAltRequest{..} = object
    [ "parent_task_id" .= upscaleAltParentTaskId
    , "type" .= upscaleAltType
    , "webhook_url" .= upscaleAltWebhookUrl
    , "webhook_secret" .= upscaleAltWebhookSecret
    ]

data UpscaleHighresRequest = UpscaleHighresRequest
  { upscaleHighresParentTaskId :: TaskId
  , upscaleHighresType :: UpscaleType
  , upscaleHighresWebhookUrl :: Maybe WebhookUrl
  , upscaleHighresWebhookSecret :: Maybe WebhookSecret
  } deriving (Show, Eq, Generic)

instance ToJSON UpscaleHighresRequest where
  toJSON UpscaleHighresRequest{..} = object
    [ "parent_task_id" .= upscaleHighresParentTaskId
    , "type" .= upscaleHighresType
    , "webhook_url" .= upscaleHighresWebhookUrl
    , "webhook_secret" .= upscaleHighresWebhookSecret
    ]

data RerollRequest = RerollRequest
  { rerollParentTaskId :: TaskId
  , rerollPrompt :: Maybe Prompt
  , rerollAspectRatio :: Maybe AspectRatio
  , rerollWebhookUrl :: Maybe WebhookUrl
  , rerollWebhookSecret :: Maybe WebhookSecret
  } deriving (Show, Eq, Generic)

instance ToJSON RerollRequest where
  toJSON RerollRequest{..} = object
    [ "parent_task_id" .= rerollParentTaskId
    , "prompt" .= rerollPrompt
    , "aspect_ratio" .= rerollAspectRatio
    , "webhook_url" .= rerollWebhookUrl
    , "webhook_secret" .= rerollWebhookSecret
    ]

data VariationsRequest = VariationsRequest
  { variationsParentTaskId :: TaskId
  , variationsIndex :: VariationsIndex -- Can be "1", "2", "3", "4", "strong", or "subtle"
  , variationsPrompt :: Maybe Prompt
  , variationsAspectRatio :: Maybe AspectRatio
  , variationsWebhookUrl :: Maybe WebhookUrl
  , variationsWebhookSecret :: Maybe WebhookSecret
  } deriving (Show, Eq, Generic)

instance ToJSON VariationsRequest where
  toJSON VariationsRequest{..} = object
    [ "parent_task_id" .= variationsParentTaskId
    , "index" .= variationsIndex
    , "prompt" .= variationsPrompt
    , "aspect_ratio" .= variationsAspectRatio
    , "webhook_url" .= variationsWebhookUrl
    , "webhook_secret" .= variationsWebhookSecret
    ]

data InpaintRequest = InpaintRequest
  { inpaintParentTaskId :: TaskId
  , inpaintMask :: Base64Image -- Base64 encoded image
  , inpaintPrompt :: Maybe Prompt
  , inpaintWebhookUrl :: Maybe WebhookUrl
  , inpaintWebhookSecret :: Maybe WebhookSecret
  } deriving (Show, Eq, Generic)

instance ToJSON InpaintRequest where
  toJSON InpaintRequest{..} = object
    [ "parent_task_id" .= inpaintParentTaskId
    , "mask" .= inpaintMask
    , "prompt" .= inpaintPrompt
    , "webhook_url" .= inpaintWebhookUrl
    , "webhook_secret" .= inpaintWebhookSecret
    ]

data OutpaintRequest = OutpaintRequest
  { outpaintParentTaskId :: TaskId
  , outpaintZoomRatio :: Double -- Can be 1, 1.5, 2 or (1, 2]
  , outpaintAspectRatio :: Maybe AspectRatio
  , outpaintPrompt :: Maybe Prompt
  , outpaintWebhookUrl :: Maybe WebhookUrl
  , outpaintWebhookSecret :: Maybe WebhookSecret
  } deriving (Show, Eq, Generic)

instance ToJSON OutpaintRequest where
  toJSON OutpaintRequest{..} = object
    [ "parent_task_id" .= outpaintParentTaskId
    , "zoom_ratio" .= outpaintZoomRatio
    , "aspect_ratio" .= outpaintAspectRatio
    , "prompt" .= outpaintPrompt
    , "webhook_url" .= outpaintWebhookUrl
    , "webhook_secret" .= outpaintWebhookSecret
    ]

data PanRequest = PanRequest
  { panParentTaskId :: TaskId
  , panDirection :: Direction
  , panPrompt :: Maybe Prompt
  , panWebhookUrl :: Maybe WebhookUrl
  , panWebhookSecret :: Maybe WebhookSecret
  } deriving (Show, Eq, Generic)

instance ToJSON PanRequest where
  toJSON PanRequest{..} = object
    [ "parent_task_id" .= panParentTaskId
    , "direction" .= panDirection
    , "prompt" .= panPrompt
    , "webhook_url" .= panWebhookUrl
    , "webhook_secret" .= panWebhookSecret
    ]

data DescribeRequest = DescribeRequest
  { describeImageUrl :: ImageUrl
  , describeProcessMode :: Maybe ProcessMode
  , describeWebhookUrl :: Maybe WebhookUrl
  , describeWebhookSecret :: Maybe WebhookSecret
  } deriving (Show, Eq, Generic)

instance ToJSON DescribeRequest where
  toJSON DescribeRequest{..} = object
    [ "image_url" .= describeImageUrl
    , "process_mode" .= describeProcessMode
    , "webhook_url" .= describeWebhookUrl
    , "webhook_secret" .= describeWebhookSecret
    ]

data BlendRequest = BlendRequest
  { blendImageUrls :: [ImageUrl] -- Min 2, max 5
  , blendDimension :: Maybe Dimension
  , blendProcessMode :: Maybe ProcessMode
  , blendWebhookUrl :: Maybe WebhookUrl
  , blendWebhookSecret :: Maybe WebhookSecret
  } deriving (Show, Eq, Generic)

instance ToJSON BlendRequest where
  toJSON BlendRequest{..} = object
    [ "image_urls" .= blendImageUrls
    , "dimension" .= blendDimension
    , "process_mode" .= blendProcessMode
    , "webhook_url" .= blendWebhookUrl
    , "webhook_secret" .= blendWebhookSecret
    ]

data SeedRequest = SeedRequest
  { seedTaskId :: TaskId
  , seedWebhookUrl :: Maybe WebhookUrl
  , seedWebhookSecret :: Maybe WebhookSecret
  } deriving (Show, Eq, Generic)

instance ToJSON SeedRequest where
  toJSON SeedRequest{..} = object
    [ "task_id" .= seedTaskId
    , "webhook_url" .= seedWebhookUrl
    , "webhook_secret" .= seedWebhookSecret
    ]

data FaceswapRequest = FaceswapRequest
  { faceswapTargetImageUrl :: ImageUrl
  , faceswapSwapImageUrl :: ImageUrl
  , faceswapWebhookUrl :: Maybe WebhookUrl
  , faceswapWebhookSecret :: Maybe WebhookSecret
  } deriving (Show, Eq, Generic)

instance ToJSON FaceswapRequest where
  toJSON FaceswapRequest{..} = object
    [ "target_image_url" .= faceswapTargetImageUrl
    , "swap_image_url" .= faceswapSwapImageUrl
    , "webhook_url" .= faceswapWebhookUrl
    , "webhook_secret" .= faceswapWebhookSecret
    ]

newtype FetchRequest = FetchRequest
  { fetchTaskId :: TaskId
  } deriving (Show, Eq, Generic)

instance ToJSON FetchRequest where
  toJSON FetchRequest{..} = object
    [ "task_id" .= fetchTaskId
    ]

newtype FetchManyRequest = FetchManyRequest
  { fetchManyTaskIds :: [TaskId] -- Min 2, max 20
  } deriving (Show, Eq, Generic)

instance ToJSON FetchManyRequest where
  toJSON FetchManyRequest{..} = object
    [ "task_ids" .= fetchManyTaskIds
    ]

-- Response Types

data TaskResponse = TaskResponse
  { taskId :: TaskId
  , taskErrors :: Maybe [ApiError]
  } deriving (Show, Eq, Generic)

instance FromJSON TaskResponse where
  parseJSON = withObject "TaskResponse" $ \v -> TaskResponse
    <$> v .: "task_id"
    <*> v .:? "errors"

-- Fetch Response Record Types

data FetchProcessing = FetchProcessing
  { fetchProcessingTaskId :: TaskId
  , fetchProcessingTaskType :: TaskType
  , fetchProcessingStatus :: TaskStatus
  , fetchProcessingPercentage :: Maybe Percentage
  } deriving (Show, Eq, Generic)

data FetchImagineComplete = FetchImagineComplete
  { fetchImagineTaskId :: TaskId
  , fetchImagineTaskType :: TaskType
  , fetchImagineSref :: Maybe Sref
  , fetchImagineOriginalImageUrl :: PngUrl
  , fetchImagineImageUrls :: [PngUrl]
  } deriving (Show, Eq, Generic)

data FetchImagineVideoComplete = FetchImagineVideoComplete
  { fetchImagineVideoTaskId :: TaskId
  , fetchImagineVideoTaskType :: TaskType
  , fetchImagineVideoUrls :: [Mp4Url]
  } deriving (Show, Eq, Generic)

data FetchRerollComplete = FetchRerollComplete
  { fetchRerollTaskId :: TaskId
  , fetchRerollTaskType :: TaskType
  , fetchRerollOriginalImageUrl :: PngUrl
  , fetchRerollImageUrls :: [PngUrl]
  } deriving (Show, Eq, Generic)

data FetchUpscaleComplete = FetchUpscaleComplete
  { fetchUpscaleTaskId :: TaskId
  , fetchUpscaleTaskType :: TaskType
  , fetchUpscaleImageUrl :: PngUrl
  } deriving (Show, Eq, Generic)

data FetchVariationComplete = FetchVariationComplete
  { fetchVariationTaskId :: TaskId
  , fetchVariationTaskType :: TaskType
  , fetchVariationOriginalImageUrl :: PngUrl
  , fetchVariationImageUrls :: [PngUrl]
  } deriving (Show, Eq, Generic)

data FetchFaceswapComplete = FetchFaceswapComplete
  { fetchFaceswapTaskId :: TaskId
  , fetchFaceswapTaskType :: TaskType
  , fetchFaceswapImageUrl :: PngUrl
  } deriving (Show, Eq, Generic)

data FetchInpaintComplete = FetchInpaintComplete
  { fetchInpaintTaskId :: TaskId
  , fetchInpaintTaskType :: TaskType
  , fetchInpaintOriginalImageUrl :: PngUrl
  , fetchInpaintImageUrls :: [PngUrl]
  } deriving (Show, Eq, Generic)

data FetchOutpaintComplete = FetchOutpaintComplete
  { fetchOutpaintTaskId :: TaskId
  , fetchOutpaintTaskType :: TaskType
  , fetchOutpaintOriginalImageUrl :: PngUrl
  , fetchOutpaintImageUrls :: [PngUrl]
  } deriving (Show, Eq, Generic)

data FetchPanComplete = FetchPanComplete
  { fetchPanTaskId :: TaskId
  , fetchPanTaskType :: TaskType
  , fetchPanOriginalImageUrl :: PngUrl
  , fetchPanImageUrls :: [PngUrl]
  } deriving (Show, Eq, Generic)

data FetchShortenComplete = FetchShortenComplete
  { fetchShortenTaskId :: TaskId
  , fetchShortenTaskType :: TaskType
  , fetchShortenContent :: [Text]
  , fetchShortenFullContent :: Maybe Text
  } deriving (Show, Eq, Generic)

data FetchDescribeComplete = FetchDescribeComplete
  { fetchDescribeTaskId :: TaskId
  , fetchDescribeTaskType :: TaskType
  , fetchDescribeImageUrl :: PngUrl
  , fetchDescribeContent :: [Text]
  } deriving (Show, Eq, Generic)

data FetchBlendComplete = FetchBlendComplete
  { fetchBlendTaskId :: TaskId
  , fetchBlendTaskType :: TaskType
  , fetchBlendOriginalImageUrl :: PngUrl
  , fetchBlendImageUrls :: [PngUrl]
  } deriving (Show, Eq, Generic)

data FetchSeedComplete = FetchSeedComplete
  { fetchSeedTaskId :: TaskId
  , fetchSeedTaskType :: TaskType
  , fetchSeedSeed :: Seed
  } deriving (Show, Eq, Generic)

data FetchFailed = FetchFailed
  { fetchFailedTaskId :: TaskId
  , fetchFailedTaskType :: TaskType
  , fetchFailedStatus :: TaskStatus
  , fetchFailedMessage :: Maybe ErrorMessage
  , fetchFailedActions :: [Text]
  } deriving (Show, Eq, Generic)

-- Fetch Response ADT

data FetchResponse
  = FetchResponseProcessing FetchProcessing
  | FetchResponseImagineComplete FetchImagineComplete
  | FetchResponseImagineVideoComplete FetchImagineVideoComplete
  | FetchResponseRerollComplete FetchRerollComplete
  | FetchResponseUpscaleComplete FetchUpscaleComplete
  | FetchResponseVariationComplete FetchVariationComplete
  | FetchResponseFaceswapComplete FetchFaceswapComplete
  | FetchResponseInpaintComplete FetchInpaintComplete
  | FetchResponseOutpaintComplete FetchOutpaintComplete
  | FetchResponsePanComplete FetchPanComplete
  | FetchResponseShortenComplete FetchShortenComplete
  | FetchResponseDescribeComplete FetchDescribeComplete
  | FetchResponseBlendComplete FetchBlendComplete
  | FetchResponseSeedComplete FetchSeedComplete
  | FetchResponseFailed FetchFailed
  deriving (Show, Eq, Generic)

instance FromJSON FetchResponse where
  parseJSON = withObject "FetchResponse" $ \v -> do
    taskId <- v .: "task_id"
    statusMaybe <- v .:? "status"
    
    -- Handle staged status specially (it doesn't have task_type)
    case statusMaybe of
      Just "staged" -> do
        -- Staged responses don't include task_type, so we use a placeholder
        pure $ FetchResponseProcessing $ FetchProcessing taskId TaskTypeImagine StatusStaged Nothing
      
      _ -> do
        -- All other statuses require task_type
        taskType <- v .: "task_type"
        
        case statusMaybe of
          Just statusText | statusText `elem` ["processing", "starting" :: Text] -> do
            let status = case statusText of
                  "processing" -> StatusProcessing
                  "starting" -> StatusStarting
                  _ -> StatusProcessing -- shouldn't happen given the guard
            percentage <- v .:? "percentage"
            pure $ FetchResponseProcessing $ FetchProcessing taskId taskType status percentage
          
          Just "failed" -> do
            message <- v .:? "message"
            actions <- v .:? "actions" .!= []
            pure $ FetchResponseFailed $ FetchFailed taskId taskType StatusFailed message actions
          
          _ -> case taskType of
            TaskTypeImagine -> do
              sref <- v .:? "sref"
              originalImageUrl <- v .: "original_image_url"
              imageUrls <- v .: "image_urls"
              pure $ FetchResponseImagineComplete $ FetchImagineComplete taskId taskType sref originalImageUrl imageUrls
            
            TaskTypeImagineVideo -> do
              videoUrls <- v .: "video_urls"
              pure $ FetchResponseImagineVideoComplete $ FetchImagineVideoComplete taskId taskType videoUrls
            
            TaskTypeReroll -> do
              originalImageUrl <- v .: "original_image_url"
              imageUrls <- v .: "image_urls"
              pure $ FetchResponseRerollComplete $ FetchRerollComplete taskId taskType originalImageUrl imageUrls
            
            TaskTypeUpscale {} -> do
              imageUrl <- v .: "image_url"
              pure $ FetchResponseUpscaleComplete $ FetchUpscaleComplete taskId taskType imageUrl
            
            TaskTypeUpscaleCreative -> do
              imageUrl <- v .: "image_url"
              pure $ FetchResponseUpscaleComplete $ FetchUpscaleComplete taskId taskType imageUrl
            
            TaskTypeUpscaleSubtle -> do
              imageUrl <- v .: "image_url"
              pure $ FetchResponseUpscaleComplete $ FetchUpscaleComplete taskId taskType imageUrl
            
            TaskTypeUpscale2x -> do
              imageUrl <- v .: "image_url"
              pure $ FetchResponseUpscaleComplete $ FetchUpscaleComplete taskId taskType imageUrl
            
            TaskTypeUpscale4x -> do
              imageUrl <- v .: "image_url"
              pure $ FetchResponseUpscaleComplete $ FetchUpscaleComplete taskId taskType imageUrl
            
            TaskTypeVariation {} -> do
              originalImageUrl <- v .: "original_image_url"
              imageUrls <- v .: "image_urls"
              pure $ FetchResponseVariationComplete $ FetchVariationComplete taskId taskType originalImageUrl imageUrls
            
            TaskTypeVariationStrong -> do
              originalImageUrl <- v .: "original_image_url"
              imageUrls <- v .: "image_urls"
              pure $ FetchResponseVariationComplete $ FetchVariationComplete taskId taskType originalImageUrl imageUrls
            
            TaskTypeVariationSubtle -> do
              originalImageUrl <- v .: "original_image_url"
              imageUrls <- v .: "image_urls"
              pure $ FetchResponseVariationComplete $ FetchVariationComplete taskId taskType originalImageUrl imageUrls
            
            TaskTypeFaceswap -> do
              imageUrl <- v .: "image_url"
              pure $ FetchResponseFaceswapComplete $ FetchFaceswapComplete taskId taskType imageUrl
            
            TaskTypeInpaint -> do
              originalImageUrl <- v .: "original_image_url"
              imageUrls <- v .: "image_urls"
              pure $ FetchResponseInpaintComplete $ FetchInpaintComplete taskId taskType originalImageUrl imageUrls
            
            TaskTypeOutpaint {} -> do
              originalImageUrl <- v .: "original_image_url"
              imageUrls <- v .: "image_urls"
              pure $ FetchResponseOutpaintComplete $ FetchOutpaintComplete taskId taskType originalImageUrl imageUrls
            
            TaskTypePan {} -> do
              originalImageUrl <- v .: "original_image_url"
              imageUrls <- v .: "image_urls"
              pure $ FetchResponsePanComplete $ FetchPanComplete taskId taskType originalImageUrl imageUrls
            
            TaskTypeShorten -> do
              content <- v .: "content"
              fullContent <- v .:? "full_content"
              pure $ FetchResponseShortenComplete $ FetchShortenComplete taskId taskType content fullContent
            
            TaskTypeDescribe -> do
              imageUrl <- v .: "image_url"
              content <- v .: "content"
              pure $ FetchResponseDescribeComplete $ FetchDescribeComplete taskId taskType imageUrl content
            
            TaskTypeBlend -> do
              originalImageUrl <- v .: "original_image_url"
              imageUrls <- v .: "image_urls"
              pure $ FetchResponseBlendComplete $ FetchBlendComplete taskId taskType originalImageUrl imageUrls
            
            TaskTypeSeed -> do
              seed <- v .: "seed"
              pure $ FetchResponseSeedComplete $ FetchSeedComplete taskId taskType seed

-- ToJSON instances for round-trip testing
instance ToJSON FetchProcessing where
  toJSON FetchProcessing{..} = object
    [ "task_id" .= fetchProcessingTaskId
    , "task_type" .= fetchProcessingTaskType
    , "status" .= fetchProcessingStatus
    , "percentage" .= fetchProcessingPercentage
    ]

instance ToJSON FetchImagineComplete where
  toJSON FetchImagineComplete{..} = object
    [ "task_id" .= fetchImagineTaskId
    , "task_type" .= fetchImagineTaskType
    , "sref" .= fetchImagineSref
    , "original_image_url" .= fetchImagineOriginalImageUrl
    , "image_urls" .= fetchImagineImageUrls
    ]

instance ToJSON FetchFailed where
  toJSON FetchFailed{..} = object
    [ "task_id" .= fetchFailedTaskId
    , "task_type" .= fetchFailedTaskType
    , "status" .= fetchFailedStatus
    , "message" .= fetchFailedMessage
    , "actions" .= fetchFailedActions
    ]

instance ToJSON FetchResponse where
  toJSON (FetchResponseProcessing processing) = toJSON processing
  toJSON (FetchResponseImagineComplete imagine) = toJSON imagine
  toJSON (FetchResponseFailed failed) = toJSON failed
  toJSON _ = error "ToJSON not implemented for all FetchResponse constructors (only needed for testing)"

newtype FetchManyResponse = FetchManyResponse
  { fetchManyTasks :: [FetchResponse]
  } deriving (Show, Eq, Generic)

instance FromJSON FetchManyResponse where
  parseJSON v = FetchManyResponse <$> parseJSON v

data AccountResponse = AccountResponse
  { accountEmail :: Email
  , accountCredits :: Double
  , accountPlan :: Plan
  , accountNextBillingDate :: Maybe Text
  , accountTotalImages :: Int
  } deriving (Show, Eq, Generic)

instance FromJSON AccountResponse where
  parseJSON = withObject "AccountResponse" $ \v -> AccountResponse
    <$> v .: "email"
    <*> v .: "credits"
    <*> v .: "plan"
    <*> v .:? "next_billing_date"
    <*> v .: "total_images"

newtype ErrorResponse = ErrorResponse
  { errorErrors :: [ApiError]
  } deriving (Show, Eq, Generic)

instance FromJSON ErrorResponse where
  parseJSON = withObject "ErrorResponse" $ \v -> ErrorResponse
    <$> v .: "errors"

newtype ApiError = ApiError
  { apiErrorMsg :: ErrorMessage
  } deriving (Show, Eq, Generic)

instance FromJSON ApiError where
  parseJSON = withObject "ApiError" $ \v -> ApiError
    <$> v .: "msg"

instance ToJSON ApiError where
  toJSON ApiError{..} = object
    [ "msg" .= apiErrorMsg
    ]

-- Enums

data AspectRatio = AspectRatio
  { aspectWidth :: Int
  , aspectHeight :: Int
  } deriving (Show, Eq, Generic)

-- | Common aspect ratios for convenience
aspectRatio1x1, aspectRatio4x3, aspectRatio3x4, aspectRatio16x9, aspectRatio9x16, aspectRatio3x2, aspectRatio2x3 :: AspectRatio
aspectRatio1x1 = AspectRatio 1 1
aspectRatio4x3 = AspectRatio 4 3
aspectRatio3x4 = AspectRatio 3 4
aspectRatio16x9 = AspectRatio 16 9
aspectRatio9x16 = AspectRatio 9 16
aspectRatio3x2 = AspectRatio 3 2
aspectRatio2x3 = AspectRatio 2 3

instance ToJSON AspectRatio where
  toJSON AspectRatio{..} = toJSON $ show aspectWidth <> ":" <> show aspectHeight

instance FromJSON AspectRatio where
  parseJSON = withText "AspectRatio" $ \t -> do
    case T.splitOn ":" t of
      [w, h] -> case (reads $ T.unpack w, reads $ T.unpack h) of
        ([(width, "")], [(height, "")]) ->
          if width > 0 && height > 0
          then pure $ AspectRatio width height
          else fail "Aspect ratio dimensions must be positive"
        _ -> fail "Invalid aspect ratio format"
      _ -> fail "Aspect ratio must be in format 'width:height'"

data ProcessMode
  = ProcessModeFast
  | ProcessModeTurbo
  deriving (Show, Eq, Generic, Bounded, Enum)

instance ToJSON ProcessMode where
  toJSON ProcessModeFast = "fast"
  toJSON ProcessModeTurbo = "turbo"

instance FromJSON ProcessMode where
  parseJSON = withText "ProcessMode" $ \case
    "fast" -> pure ProcessModeFast
    "turbo" -> pure ProcessModeTurbo
    _ -> fail "Invalid process mode"

data UpscaleType
  = Upscale2x
  | Upscale4x
  deriving (Show, Eq, Generic, Bounded, Enum)

instance ToJSON UpscaleType where
  toJSON Upscale2x = "2x"
  toJSON Upscale4x = "4x"

instance FromJSON UpscaleType where
  parseJSON = withText "UpscaleType" $ \case
    "2x" -> pure Upscale2x
    "4x" -> pure Upscale4x
    _ -> fail "Invalid upscale type"

data UpscaleAltType
  = UpscaleSubtle
  | UpscaleCreative
  deriving (Show, Eq, Generic, Bounded, Enum)

instance ToJSON UpscaleAltType where
  toJSON UpscaleSubtle = "subtle"
  toJSON UpscaleCreative = "creative"

instance FromJSON UpscaleAltType where
  parseJSON = withText "UpscaleAltType" $ \case
    "subtle" -> pure UpscaleSubtle
    "creative" -> pure UpscaleCreative
    _ -> fail "Invalid upscale alt type"

data ImageIndex
  = Index1
  | Index2
  | Index3
  | Index4
  deriving (Show, Eq, Generic, Bounded, Enum)

instance ToJSON ImageIndex where
  toJSON Index1 = "1"
  toJSON Index2 = "2"
  toJSON Index3 = "3"
  toJSON Index4 = "4"

instance FromJSON ImageIndex where
  parseJSON = withText "ImageIndex" $ \case
    "1" -> pure Index1
    "2" -> pure Index2
    "3" -> pure Index3
    "4" -> pure Index4
    _ -> fail "Invalid image index"

data Direction
  = DirectionUp
  | DirectionDown
  | DirectionLeft
  | DirectionRight
  deriving (Show, Eq, Generic, Bounded, Enum)

instance ToJSON Direction where
  toJSON DirectionUp = "up"
  toJSON DirectionDown = "down"
  toJSON DirectionLeft = "left"
  toJSON DirectionRight = "right"

instance FromJSON Direction where
  parseJSON = withText "Direction" $ \case
    "up" -> pure DirectionUp
    "down" -> pure DirectionDown
    "left" -> pure DirectionLeft
    "right" -> pure DirectionRight
    _ -> fail "Invalid direction"

data Dimension
  = DimensionSquare
  | DimensionPortrait
  | DimensionLandscape
  deriving (Show, Eq, Generic, Bounded, Enum)

instance ToJSON Dimension where
  toJSON DimensionSquare = "square"
  toJSON DimensionPortrait = "portrait"
  toJSON DimensionLandscape = "landscape"

instance FromJSON Dimension where
  parseJSON = withText "Dimension" $ \case
    "square" -> pure DimensionSquare
    "portrait" -> pure DimensionPortrait
    "landscape" -> pure DimensionLandscape
    _ -> fail "Invalid dimension"

data TaskStatus
  = StatusPending
  | StatusStaged
  | StatusStarting
  | StatusProcessing
  | StatusFinished
  | StatusFailed
  | StatusRetry
  | StatusRetrying
  deriving (Show, Eq, Generic, Bounded, Enum)

instance FromJSON TaskStatus where
  parseJSON = withText "TaskStatus" $ \case
    "pending" -> pure StatusPending
    "staged" -> pure StatusStaged
    "starting" -> pure StatusStarting
    "processing" -> pure StatusProcessing
    "finished" -> pure StatusFinished
    "failed" -> pure StatusFailed
    "retry" -> pure StatusRetry
    "retrying" -> pure StatusRetrying
    _ -> fail "Invalid task status"

instance ToJSON TaskStatus where
  toJSON StatusPending = "pending"
  toJSON StatusStaged = "staged"
  toJSON StatusStarting = "starting"
  toJSON StatusProcessing = "processing"
  toJSON StatusFinished = "finished"
  toJSON StatusFailed = "failed"
  toJSON StatusRetry = "retry"
  toJSON StatusRetrying = "retrying"

data TaskType
  = TaskTypeImagine
  | TaskTypeImagineVideo
  | TaskTypeReroll
  | TaskTypeUpscale ImageIndex -- e.g., upscale#1, upscale#2
  | TaskTypeUpscaleCreative
  | TaskTypeUpscaleSubtle
  | TaskTypeUpscale2x
  | TaskTypeUpscale4x
  | TaskTypeVariation ImageIndex -- e.g., variation-1
  | TaskTypeVariationStrong
  | TaskTypeVariationSubtle
  | TaskTypeFaceswap
  | TaskTypeInpaint
  | TaskTypeOutpaint Int -- e.g., outpaint-2
  | TaskTypePan Direction -- e.g., pan-up, pan-down
  | TaskTypeShorten
  | TaskTypeDescribe
  | TaskTypeBlend
  | TaskTypeSeed
  deriving (Show, Eq, Generic)

instance FromJSON TaskType where
  parseJSON = withText "TaskType" $ \t -> case t of
    "imagine" -> pure TaskTypeImagine
    "imagine-video" -> pure TaskTypeImagineVideo
    "reroll" -> pure TaskTypeReroll
    "upscale#1" -> pure $ TaskTypeUpscale Index1
    "upscale#2" -> pure $ TaskTypeUpscale Index2
    "upscale#3" -> pure $ TaskTypeUpscale Index3
    "upscale#4" -> pure $ TaskTypeUpscale Index4
    "upscale-creative" -> pure TaskTypeUpscaleCreative
    "upscale-subtle" -> pure TaskTypeUpscaleSubtle
    "upscale-2x" -> pure TaskTypeUpscale2x
    "upscale-4x" -> pure TaskTypeUpscale4x
    "variation-1" -> pure $ TaskTypeVariation Index1
    "variation-2" -> pure $ TaskTypeVariation Index2
    "variation-3" -> pure $ TaskTypeVariation Index3
    "variation-4" -> pure $ TaskTypeVariation Index4
    "variation-strong" -> pure TaskTypeVariationStrong
    "variation-subtle" -> pure TaskTypeVariationSubtle
    "faceswap" -> pure TaskTypeFaceswap
    "inpaint" -> pure TaskTypeInpaint
    "pan-up" -> pure $ TaskTypePan DirectionUp
    "pan-down" -> pure $ TaskTypePan DirectionDown
    "pan-left" -> pure $ TaskTypePan DirectionLeft
    "pan-right" -> pure $ TaskTypePan DirectionRight
    "shorten" -> pure TaskTypeShorten
    "describe" -> pure TaskTypeDescribe
    "blend" -> pure TaskTypeBlend
    "seed" -> pure TaskTypeSeed
    s | T.isPrefixOf "outpaint-" s -> do
      let numStr = T.drop 9 s
      case reads (T.unpack numStr) of
        [(n, "")] -> pure $ TaskTypeOutpaint n
        _ -> fail $ "Invalid outpaint type: " <> T.unpack t
    _ -> fail $ "Unknown task type: " <> T.unpack t

instance ToJSON TaskType where
  toJSON TaskTypeImagine = "imagine"
  toJSON TaskTypeImagineVideo = "imagine-video"
  toJSON TaskTypeReroll = "reroll"
  toJSON (TaskTypeUpscale Index1) = "upscale#1"
  toJSON (TaskTypeUpscale Index2) = "upscale#2"
  toJSON (TaskTypeUpscale Index3) = "upscale#3"
  toJSON (TaskTypeUpscale Index4) = "upscale#4"
  toJSON TaskTypeUpscaleCreative = "upscale-creative"
  toJSON TaskTypeUpscaleSubtle = "upscale-subtle"
  toJSON TaskTypeUpscale2x = "upscale-2x"
  toJSON TaskTypeUpscale4x = "upscale-4x"
  toJSON (TaskTypeVariation Index1) = "variation-1"
  toJSON (TaskTypeVariation Index2) = "variation-2"
  toJSON (TaskTypeVariation Index3) = "variation-3"
  toJSON (TaskTypeVariation Index4) = "variation-4"
  toJSON TaskTypeVariationStrong = "variation-strong"
  toJSON TaskTypeVariationSubtle = "variation-subtle"
  toJSON TaskTypeFaceswap = "faceswap"
  toJSON TaskTypeInpaint = "inpaint"
  toJSON (TaskTypeOutpaint n) = toJSON $ "outpaint-" <> T.pack (show n)
  toJSON (TaskTypePan DirectionUp) = "pan-up"
  toJSON (TaskTypePan DirectionDown) = "pan-down"
  toJSON (TaskTypePan DirectionLeft) = "pan-left"
  toJSON (TaskTypePan DirectionRight) = "pan-right"
  toJSON TaskTypeShorten = "shorten"
  toJSON TaskTypeDescribe = "describe"
  toJSON TaskTypeBlend = "blend"
  toJSON TaskTypeSeed = "seed"

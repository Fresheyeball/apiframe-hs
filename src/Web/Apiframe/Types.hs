{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Web.Apiframe.Types where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import Data.Time (UTCTime)

-- Newtype wrappers for domain-specific text values

newtype TaskId = TaskId Text
  deriving stock (Generic)
  deriving newtype (Eq, Show, Read, ToJSON, FromJSON)

newtype Prompt = Prompt Text
  deriving stock (Generic)
  deriving newtype (Eq, Show, Read, ToJSON, FromJSON)

newtype ImageUrl = ImageUrl Text
  deriving stock (Generic)
  deriving newtype (Eq, Show, Read, ToJSON, FromJSON)

newtype Base64Image = Base64Image Text
  deriving stock (Generic)
  deriving newtype (Eq, Show, Read, ToJSON, FromJSON)

newtype VariationsIndex = VariationsIndex Text
  deriving stock (Generic)
  deriving newtype (Eq, Show, Read, ToJSON, FromJSON)

newtype WebhookUrl = WebhookUrl Text
  deriving stock (Generic)
  deriving newtype (Eq, Show, Read, ToJSON, FromJSON)

newtype WebhookSecret = WebhookSecret Text
  deriving stock (Generic)
  deriving newtype (Eq, Show, Read, ToJSON, FromJSON)

newtype Email = Email Text
  deriving stock (Generic)
  deriving newtype (Eq, Show, Read, ToJSON, FromJSON)

newtype Plan = Plan Text
  deriving stock (Generic)
  deriving newtype (Eq, Show, Read, ToJSON, FromJSON)

newtype ErrorMessage = ErrorMessage Text
  deriving stock (Generic)
  deriving newtype (Eq, Show, Read, ToJSON, FromJSON)

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

data FetchRequest = FetchRequest
  { fetchTaskId :: TaskId
  } deriving (Show, Eq, Generic)

instance ToJSON FetchRequest where
  toJSON FetchRequest{..} = object
    [ "task_id" .= fetchTaskId
    ]

data FetchManyRequest = FetchManyRequest
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

data FetchResponse = FetchResponse
  { fetchTaskId :: TaskId
  , fetchStatus :: TaskStatus
  , fetchPrompt :: Maybe Prompt
  , fetchResult :: Maybe Value -- Can contain various result data
  , fetchCreatedAt :: Maybe UTCTime
  , fetchUpdatedAt :: Maybe UTCTime
  } deriving (Show, Eq, Generic)

instance FromJSON FetchResponse where
  parseJSON = withObject "FetchResponse" $ \v -> FetchResponse
    <$> v .: "task_id"
    <*> v .: "status"
    <*> v .:? "prompt"
    <*> v .:? "result"
    <*> v .:? "created_at"
    <*> v .:? "updated_at"

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

data ErrorResponse = ErrorResponse
  { errorErrors :: [ApiError]
  } deriving (Show, Eq, Generic)

instance FromJSON ErrorResponse where
  parseJSON = withObject "ErrorResponse" $ \v -> ErrorResponse
    <$> v .: "errors"

data ApiError = ApiError
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
  | StatusProcessing
  | StatusCompleted
  | StatusFailed
  deriving (Show, Eq, Generic, Bounded, Enum)

instance FromJSON TaskStatus where
  parseJSON = withText "TaskStatus" $ \case
    "pending" -> pure StatusPending
    "processing" -> pure StatusProcessing
    "completed" -> pure StatusCompleted
    "failed" -> pure StatusFailed
    _ -> fail "Invalid task status"

instance ToJSON TaskStatus where
  toJSON StatusPending = "pending"
  toJSON StatusProcessing = "processing"
  toJSON StatusCompleted = "completed"
  toJSON StatusFailed = "failed"

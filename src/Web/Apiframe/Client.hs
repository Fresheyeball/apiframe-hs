module Web.Apiframe.Client
  ( -- * Client Configuration
    ApiframeClient(..)
  , mkApiframeClient
  , runApiframe
  
    -- * API Methods
  , imagine
  , upscale1x
  , upscaleAlt
  , upscaleHighres
  , reroll
  , variations
  , inpaint
  , outpaint
  , pan
  , describe
  , blend
  , seed
  , faceswap
  , fetch
  , fetchMany
  , account
  
    -- * Re-exports
  , module Web.Apiframe.Types
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client
import Web.Apiframe.API
import Web.Apiframe.Types

-- | Configuration for the Apiframe client
data ApiframeClient = ApiframeClient
  { apiframeApiKey :: Text
  , apiframeManager :: Manager
  , apiframeBaseUrl :: BaseUrl
  }

-- | Create a new Apiframe client with the given API key
mkApiframeClient :: MonadIO m => Text -> m ApiframeClient
mkApiframeClient apiKey = do
  manager <- liftIO $ newManager tlsManagerSettings
  return ApiframeClient
    { apiframeApiKey = apiKey
    , apiframeManager = manager
    , apiframeBaseUrl = BaseUrl Https "api.apiframe.pro" 443 ""
    }

-- | Run an Apiframe API call
runApiframe :: ApiframeClient -> ClientM a -> IO (Either ClientError a)
runApiframe ApiframeClient{..} action = 
  runClientM action (mkClientEnv apiframeManager apiframeBaseUrl)

-- Generate client functions from the API definition
imagineClient :: Text -> ImagineRequest -> ClientM TaskResponse
upscale1xClient :: Text -> UpscaleRequest -> ClientM TaskResponse
upscaleAltClient :: Text -> UpscaleAltRequest -> ClientM TaskResponse
upscaleHighresClient :: Text -> UpscaleHighresRequest -> ClientM TaskResponse
rerollClient :: Text -> RerollRequest -> ClientM TaskResponse
variationsClient :: Text -> VariationsRequest -> ClientM TaskResponse
inpaintClient :: Text -> InpaintRequest -> ClientM TaskResponse
outpaintClient :: Text -> OutpaintRequest -> ClientM TaskResponse
panClient :: Text -> PanRequest -> ClientM TaskResponse
describeClient :: Text -> DescribeRequest -> ClientM TaskResponse
blendClient :: Text -> BlendRequest -> ClientM TaskResponse
seedClient :: Text -> SeedRequest -> ClientM TaskResponse
faceswapClient :: Text -> FaceswapRequest -> ClientM TaskResponse
fetchClient :: Text -> FetchRequest -> ClientM FetchResponse
fetchManyClient :: Text -> FetchManyRequest -> ClientM FetchManyResponse
accountClient :: Text -> ClientM AccountResponse

imagineClient
  :<|> upscale1xClient
  :<|> upscaleAltClient
  :<|> upscaleHighresClient
  :<|> rerollClient
  :<|> variationsClient
  :<|> inpaintClient
  :<|> outpaintClient
  :<|> panClient
  :<|> describeClient
  :<|> blendClient
  :<|> seedClient
  :<|> faceswapClient
  :<|> fetchClient
  :<|> fetchManyClient
  :<|> accountClient = client apiframeAPI

-- | Generate an image using a text prompt (the /imagine command on Discord)
imagine :: ApiframeClient -> ImagineRequest -> IO (Either ClientError TaskResponse)
imagine apiClient@ApiframeClient{..} req = 
  runApiframe apiClient (imagineClient apiframeApiKey req)

-- | Upscale one of the 4 generated images to get a single image
upscale1x :: ApiframeClient -> UpscaleRequest -> IO (Either ClientError TaskResponse)
upscale1x apiClient@ApiframeClient{..} req = 
  runApiframe apiClient (upscale1xClient apiframeApiKey req)

-- | Upscale with Subtle or Creative options (requires prior 1x upscale)
upscaleAlt :: ApiframeClient -> UpscaleAltRequest -> IO (Either ClientError TaskResponse)
upscaleAlt apiClient@ApiframeClient{..} req = 
  runApiframe apiClient (upscaleAltClient apiframeApiKey req)

-- | Upscale any image to higher resolution (not from Midjourney)
upscaleHighres :: ApiframeClient -> UpscaleHighresRequest -> IO (Either ClientError TaskResponse)
upscaleHighres apiClient@ApiframeClient{..} req = 
  runApiframe apiClient (upscaleHighresClient apiframeApiKey req)

-- | Reroll to create new images from a previous Imagine task
reroll :: ApiframeClient -> RerollRequest -> IO (Either ClientError TaskResponse)
reroll apiClient@ApiframeClient{..} req = 
  runApiframe apiClient (rerollClient apiframeApiKey req)

-- | Create 4 new variations of one of the generated images
variations :: ApiframeClient -> VariationsRequest -> IO (Either ClientError TaskResponse)
variations apiClient@ApiframeClient{..} req = 
  runApiframe apiClient (variationsClient apiframeApiKey req)

-- | Redraw a selected area of an image (Vary Region)
inpaint :: ApiframeClient -> InpaintRequest -> IO (Either ClientError TaskResponse)
inpaint apiClient@ApiframeClient{..} req = 
  runApiframe apiClient (inpaintClient apiframeApiKey req)

-- | Enlarge an image's canvas beyond its original size (Zoom Out)
outpaint :: ApiframeClient -> OutpaintRequest -> IO (Either ClientError TaskResponse)
outpaint apiClient@ApiframeClient{..} req = 
  runApiframe apiClient (outpaintClient apiframeApiKey req)

-- | Broaden the image canvas in a specific direction
pan :: ApiframeClient -> PanRequest -> IO (Either ClientError TaskResponse)
pan apiClient@ApiframeClient{..} req = 
  runApiframe apiClient (panClient apiframeApiKey req)

-- | Write four example prompts based on an uploaded image (/describe command)
describe :: ApiframeClient -> DescribeRequest -> IO (Either ClientError TaskResponse)
describe apiClient@ApiframeClient{..} req = 
  runApiframe apiClient (describeClient apiframeApiKey req)

-- | Blend multiple images into one image
blend :: ApiframeClient -> BlendRequest -> IO (Either ClientError TaskResponse)
blend apiClient@ApiframeClient{..} req = 
  runApiframe apiClient (blendClient apiframeApiKey req)

-- | Get the seed of a generated image
seed :: ApiframeClient -> SeedRequest -> IO (Either ClientError TaskResponse)
seed apiClient@ApiframeClient{..} req = 
  runApiframe apiClient (seedClient apiframeApiKey req)

-- | Swap the face on a target image with a provided face
faceswap :: ApiframeClient -> FaceswapRequest -> IO (Either ClientError TaskResponse)
faceswap apiClient@ApiframeClient{..} req = 
  runApiframe apiClient (faceswapClient apiframeApiKey req)

-- | Get the result/status of a submitted task
fetch :: ApiframeClient -> FetchRequest -> IO (Either ClientError FetchResponse)
fetch apiClient@ApiframeClient{..} req = 
  runApiframe apiClient (fetchClient apiframeApiKey req)

-- | Get the results/statuses of multiple tasks
fetchMany :: ApiframeClient -> FetchManyRequest -> IO (Either ClientError FetchManyResponse)
fetchMany apiClient@ApiframeClient{..} req = 
  runApiframe apiClient (fetchManyClient apiframeApiKey req)

-- | Get details about your account: credits, stats, etc.
account :: ApiframeClient -> IO (Either ClientError AccountResponse)
account apiClient@ApiframeClient{..} = 
  runApiframe apiClient (accountClient apiframeApiKey)
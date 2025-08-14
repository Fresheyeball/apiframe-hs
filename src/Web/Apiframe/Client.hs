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

import Control.Monad.IO.Class (MonadIO)
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
imagine client@ApiframeClient{..} req = 
  runApiframe client (imagineClient apiframeApiKey req)

-- | Upscale one of the 4 generated images to get a single image
upscale1x :: ApiframeClient -> UpscaleRequest -> IO (Either ClientError TaskResponse)
upscale1x client@ApiframeClient{..} req = 
  runApiframe client (upscale1xClient apiframeApiKey req)

-- | Upscale with Subtle or Creative options (requires prior 1x upscale)
upscaleAlt :: ApiframeClient -> UpscaleAltRequest -> IO (Either ClientError TaskResponse)
upscaleAlt client@ApiframeClient{..} req = 
  runApiframe client (upscaleAltClient apiframeApiKey req)

-- | Upscale any image to higher resolution (not from Midjourney)
upscaleHighres :: ApiframeClient -> UpscaleHighresRequest -> IO (Either ClientError TaskResponse)
upscaleHighres client@ApiframeClient{..} req = 
  runApiframe client (upscaleHighresClient apiframeApiKey req)

-- | Reroll to create new images from a previous Imagine task
reroll :: ApiframeClient -> RerollRequest -> IO (Either ClientError TaskResponse)
reroll client@ApiframeClient{..} req = 
  runApiframe client (rerollClient apiframeApiKey req)

-- | Create 4 new variations of one of the generated images
variations :: ApiframeClient -> VariationsRequest -> IO (Either ClientError TaskResponse)
variations client@ApiframeClient{..} req = 
  runApiframe client (variationsClient apiframeApiKey req)

-- | Redraw a selected area of an image (Vary Region)
inpaint :: ApiframeClient -> InpaintRequest -> IO (Either ClientError TaskResponse)
inpaint client@ApiframeClient{..} req = 
  runApiframe client (inpaintClient apiframeApiKey req)

-- | Enlarge an image's canvas beyond its original size (Zoom Out)
outpaint :: ApiframeClient -> OutpaintRequest -> IO (Either ClientError TaskResponse)
outpaint client@ApiframeClient{..} req = 
  runApiframe client (outpaintClient apiframeApiKey req)

-- | Broaden the image canvas in a specific direction
pan :: ApiframeClient -> PanRequest -> IO (Either ClientError TaskResponse)
pan client@ApiframeClient{..} req = 
  runApiframe client (panClient apiframeApiKey req)

-- | Write four example prompts based on an uploaded image (/describe command)
describe :: ApiframeClient -> DescribeRequest -> IO (Either ClientError TaskResponse)
describe client@ApiframeClient{..} req = 
  runApiframe client (describeClient apiframeApiKey req)

-- | Blend multiple images into one image
blend :: ApiframeClient -> BlendRequest -> IO (Either ClientError TaskResponse)
blend client@ApiframeClient{..} req = 
  runApiframe client (blendClient apiframeApiKey req)

-- | Get the seed of a generated image
seed :: ApiframeClient -> SeedRequest -> IO (Either ClientError TaskResponse)
seed client@ApiframeClient{..} req = 
  runApiframe client (seedClient apiframeApiKey req)

-- | Swap the face on a target image with a provided face
faceswap :: ApiframeClient -> FaceswapRequest -> IO (Either ClientError TaskResponse)
faceswap client@ApiframeClient{..} req = 
  runApiframe client (faceswapClient apiframeApiKey req)

-- | Get the result/status of a submitted task
fetch :: ApiframeClient -> FetchRequest -> IO (Either ClientError FetchResponse)
fetch client@ApiframeClient{..} req = 
  runApiframe client (fetchClient apiframeApiKey req)

-- | Get the results/statuses of multiple tasks
fetchMany :: ApiframeClient -> FetchManyRequest -> IO (Either ClientError FetchManyResponse)
fetchMany client@ApiframeClient{..} req = 
  runApiframe client (fetchManyClient apiframeApiKey req)

-- | Get details about your account: credits, stats, etc.
account :: ApiframeClient -> IO (Either ClientError AccountResponse)
account client@ApiframeClient{..} = 
  runApiframe client (accountClient apiframeApiKey)
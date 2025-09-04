module Web.Apiframe.API
  ( ApiframeAPI
  , apiframeAPI
  ) where

import Data.Proxy
import Servant.API
import Web.Apiframe.Types

-- | Type-level API definition for APIFRAME.PRO
type ApiframeAPI = 
       -- Imagine endpoint
       "imagine" 
       :> Header' '[Required, Strict] "Authorization" ApiKey
       :> ReqBody '[JSON] ImagineRequest
       :> Post '[JSON] TaskResponse

  :<|> -- Upscale 1x endpoint
       "upscale-1x"
       :> Header' '[Required, Strict] "Authorization" ApiKey
       :> ReqBody '[JSON] UpscaleRequest
       :> Post '[JSON] TaskResponse

  :<|> -- Upscale Alt endpoint
       "upscale-alt"
       :> Header' '[Required, Strict] "Authorization" ApiKey
       :> ReqBody '[JSON] UpscaleAltRequest
       :> Post '[JSON] TaskResponse

  :<|> -- Upscale Highres endpoint
       "upscale-highres"
       :> Header' '[Required, Strict] "Authorization" ApiKey
       :> ReqBody '[JSON] UpscaleHighresRequest
       :> Post '[JSON] TaskResponse

  :<|> -- Reroll endpoint
       "reroll"
       :> Header' '[Required, Strict] "Authorization" ApiKey
       :> ReqBody '[JSON] RerollRequest
       :> Post '[JSON] TaskResponse

  :<|> -- Variations endpoint
       "variations"
       :> Header' '[Required, Strict] "Authorization" ApiKey
       :> ReqBody '[JSON] VariationsRequest
       :> Post '[JSON] TaskResponse

  :<|> -- Inpaint endpoint
       "inpaint"
       :> Header' '[Required, Strict] "Authorization" ApiKey
       :> ReqBody '[JSON] InpaintRequest
       :> Post '[JSON] TaskResponse

  :<|> -- Outpaint endpoint
       "outpaint"
       :> Header' '[Required, Strict] "Authorization" ApiKey
       :> ReqBody '[JSON] OutpaintRequest
       :> Post '[JSON] TaskResponse

  :<|> -- Pan endpoint
       "pan"
       :> Header' '[Required, Strict] "Authorization" ApiKey
       :> ReqBody '[JSON] PanRequest
       :> Post '[JSON] TaskResponse

  :<|> -- Describe endpoint
       "describe"
       :> Header' '[Required, Strict] "Authorization" ApiKey
       :> ReqBody '[JSON] DescribeRequest
       :> Post '[JSON] TaskResponse

  :<|> -- Blend endpoint
       "blend"
       :> Header' '[Required, Strict] "Authorization" ApiKey
       :> ReqBody '[JSON] BlendRequest
       :> Post '[JSON] TaskResponse

  :<|> -- Seed endpoint
       "seed"
       :> Header' '[Required, Strict] "Authorization" ApiKey
       :> ReqBody '[JSON] SeedRequest
       :> Post '[JSON] TaskResponse

  :<|> -- Faceswap endpoint
       "faceswap"
       :> Header' '[Required, Strict] "Authorization" ApiKey
       :> ReqBody '[JSON] FaceswapRequest
       :> Post '[JSON] TaskResponse

  :<|> -- Fetch endpoint
       "fetch"
       :> Header' '[Required, Strict] "Authorization" ApiKey
       :> ReqBody '[JSON] FetchRequest
       :> Post '[JSON] FetchResponse

  :<|> -- Fetch Many endpoint
       "fetch-many"
       :> Header' '[Required, Strict] "Authorization" ApiKey
       :> ReqBody '[JSON] FetchManyRequest
       :> Post '[JSON] FetchManyResponse

  :<|> -- Account endpoint
       "account"
       :> Header' '[Required, Strict] "Authorization" ApiKey
       :> Get '[JSON] AccountResponse

-- | Proxy for the API
apiframeAPI :: Proxy ApiframeAPI
apiframeAPI = Proxy
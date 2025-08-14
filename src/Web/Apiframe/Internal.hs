module Web.Apiframe.Internal
  ( -- * Internal utilities
    defaultAspectRatio
  , defaultProcessMode
  ) where

import Web.Apiframe.Types

-- | Default aspect ratio for image generation (1:1 square)
defaultAspectRatio :: AspectRatio
defaultAspectRatio = AspectRatio 1 1

-- | Default process mode for image generation
defaultProcessMode :: ProcessMode
defaultProcessMode = ProcessModeFast
module Web.Apiframe.Internal
  ( -- * Internal utilities
    defaultAspectRatio
  , defaultProcessMode
  ) where

import Web.Apiframe.Types

-- | Default aspect ratio for image generation
defaultAspectRatio :: AspectRatio
defaultAspectRatio = AspectRatio1x1

-- | Default process mode for image generation
defaultProcessMode :: ProcessMode
defaultProcessMode = ProcessModeFast
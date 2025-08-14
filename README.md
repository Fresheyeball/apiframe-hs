# apiframe-hs

[![Haskell CI](https://img.shields.io/badge/Haskell-CI-blue.svg)](https://github.com/yourusername/apiframe-hs)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

Type-safe Haskell client library for [APIFRAME.PRO](https://apiframe.pro) - programmatic access to Midjourney AI image generation.

## Features

- **Type-safe API**: Built with Servant for compile-time API verification
- **Property-tested**: Comprehensive QuickCheck tests for JSON serialization
- **Flexible types**: AspectRatio as integer pairs, comprehensive enum types
- **Async support**: Webhook integration for long-running operations
- **Complete coverage**: All APIFRAME.PRO endpoints supported

## Installation

Add to your `package.yaml` or `.cabal` file:

```yaml
dependencies:
  - apiframe-hs
```

Or with Cabal:

```bash
cabal install apiframe-hs
```

## Quick Start

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Web.Apiframe.Client
import System.Environment (getEnv)

main :: IO ()
main = do
  apiKey <- getEnv "APIFRAME_API_KEY"  
  client <- mkApiframeClient apiKey
  
  -- Generate an image
  let request = ImagineRequest
        { imaginePrompt = "a nice day in the desert with my dog"
        , imagineAspectRatio = Just (AspectRatio 3 2)  -- 3:2 ratio
        , imagineProcessMode = Just ProcessModeFast
        , imagineWebhookUrl = Nothing
        , imagineWebhookSecret = Nothing
        }
  
  result <- imagine client request
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right TaskResponse{..} -> putStrLn $ "Task ID: " ++ show taskId
```

## API Reference

### Client Operations

#### Image Generation
```haskell
imagine :: ApiframeClient -> ImagineRequest -> IO (Either ClientError TaskResponse)
```

#### Upscaling Operations
```haskell
upscale1x :: ApiframeClient -> UpscaleRequest -> IO (Either ClientError TaskResponse)
upscaleAlt :: ApiframeClient -> UpscaleAltRequest -> IO (Either ClientError TaskResponse)
upscaleHighres :: ApiframeClient -> UpscaleHighresRequest -> IO (Either ClientError TaskResponse)
```

#### Image Variations & Editing
```haskell
variations :: ApiframeClient -> VariationsRequest -> IO (Either ClientError TaskResponse)
reroll :: ApiframeClient -> RerollRequest -> IO (Either ClientError TaskResponse)
inpaint :: ApiframeClient -> InpaintRequest -> IO (Either ClientError TaskResponse)
outpaint :: ApiframeClient -> OutpaintRequest -> IO (Either ClientError TaskResponse)
pan :: ApiframeClient -> PanRequest -> IO (Either ClientError TaskResponse)
```

#### Utility Operations
```haskell
describe :: ApiframeClient -> DescribeRequest -> IO (Either ClientError TaskResponse)
blend :: ApiframeClient -> BlendRequest -> IO (Either ClientError TaskResponse)
seed :: ApiframeClient -> SeedRequest -> IO (Either ClientError TaskResponse)
faceswap :: ApiframeClient -> FaceswapRequest -> IO (Either ClientError TaskResponse)
```

#### Status & Management
```haskell
fetch :: ApiframeClient -> FetchRequest -> IO (Either ClientError FetchResponse)
fetchMany :: ApiframeClient -> FetchManyRequest -> IO (Either ClientError FetchManyResponse)
account :: ApiframeClient -> IO (Either ClientError AccountResponse)
```

### Types

#### AspectRatio
```haskell
data AspectRatio = AspectRatio
  { aspectWidth :: Int
  , aspectHeight :: Int
  }

-- Convenience constructors
aspectRatio1x1, aspectRatio4x3, aspectRatio16x9 :: AspectRatio
```

#### Process Modes
```haskell
data ProcessMode = ProcessModeFast | ProcessModeTurbo
```

#### Image Indices
```haskell
data ImageIndex = Index1 | Index2 | Index3 | Index4
```

#### Task Status
```haskell
data TaskStatus = StatusPending | StatusProcessing | StatusCompleted | StatusFailed
```

## Examples

### Basic Image Generation with Webhook

```haskell
import Web.Apiframe.Client
import Data.Text (pack)

generateImage :: IO ()
generateImage = do
  client <- mkApiframeClient "your-api-key"
  
  let request = ImagineRequest
        { imaginePrompt = "cyberpunk city at sunset, neon lights"
        , imagineAspectRatio = Just aspectRatio16x9
        , imagineProcessMode = Just ProcessModeTurbo
        , imagineWebhookUrl = Just "https://myapp.com/webhook"
        , imagineWebhookSecret = Just "secret-key"
        }
  
  result <- imagine client request
  case result of
    Left err -> print err
    Right response -> putStrLn $ "Started task: " ++ show (taskId response)
```

### Upscaling an Image

```haskell
upscaleImage :: Text -> IO ()
upscaleImage parentTaskId = do
  client <- mkApiframeClient "your-api-key"
  
  let request = UpscaleRequest
        { upscaleParentTaskId = parentTaskId
        , upscaleIndex = Index1  -- Upscale first image
        , upscaleWebhookUrl = Nothing
        , upscaleWebhookSecret = Nothing
        }
  
  result <- upscale1x client request
  case result of
    Left err -> print err
    Right response -> putStrLn $ "Upscale task: " ++ show (taskId response)
```

### Checking Task Status

```haskell
checkStatus :: Text -> IO ()
checkStatus taskId = do
  client <- mkApiframeClient "your-api-key"
  
  let request = FetchRequest { fetchTaskId = taskId }
  
  result <- fetch client request
  case result of
    Left err -> print err
    Right response -> do
      putStrLn $ "Status: " ++ show (fetchStatus response)
      case fetchResult response of
        Just result -> putStrLn $ "Result: " ++ show result
        Nothing -> putStrLn "No result yet"
```

### Account Information

```haskell
checkAccount :: IO ()
checkAccount = do
  client <- mkApiframeClient "your-api-key"
  
  result <- account client
  case result of
    Left err -> print err
    Right AccountResponse{..} -> do
      putStrLn $ "Email: " ++ show accountEmail
      putStrLn $ "Credits: " ++ show accountCredits
      putStrLn $ "Plan: " ++ show accountPlan
      putStrLn $ "Total images: " ++ show accountTotalImages
```

## Error Handling

The library uses `Either ClientError a` for all API calls:

```haskell
case result of
  Left (FailureResponse _ response) -> 
    putStrLn $ "HTTP error: " ++ show response
  Left (DecodeFailure text _) -> 
    putStrLn $ "JSON decode error: " ++ show text
  Left (UnsupportedContentType mediaType _) -> 
    putStrLn $ "Unsupported content type: " ++ show mediaType
  Left (InvalidContentTypeHeader _) -> 
    putStrLn "Invalid content type header"
  Left (ConnectionError exception) -> 
    putStrLn $ "Connection error: " ++ show exception
  Right value -> 
    -- Handle success
```

## Configuration

Set your API key as an environment variable:

```bash
export APIFRAME_API_KEY="your-api-key-here"
```

Or pass it directly when creating the client:

```haskell
client <- mkApiframeClient "your-api-key"
```

## Testing

Run the test suite:

```bash
cabal test
```

The library includes comprehensive property-based tests using QuickCheck to verify JSON serialization round-trips for all data types.

## Development

Building from source:

```bash
git clone https://github.com/yourusername/apiframe-hs
cd apiframe-hs
cabal build
```

## Documentation

- [APIFRAME.PRO API Documentation](https://docs.apiframe.pro)
- [Haskell API Documentation](https://hackage.haskell.org/package/apiframe-hs)

## License

MIT License. See [LICENSE](LICENSE) file for details.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass: `cabal test`
5. Submit a pull request

## Support

- 📧 Email: support@apiframe.pro
- 📖 Documentation: https://docs.apiframe.pro
- 🐛 Issues: https://github.com/yourusername/apiframe-hs/issues
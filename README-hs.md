# apiframe-hs

Haskell client library for [APIFRAME.PRO](https://apiframe.pro) (Midjourney API)

This library provides a type-safe Haskell interface to the APIFRAME.PRO service using the Servant library for automatic client generation.

## Features

- Type-safe API using Servant
- Automatic client generation from API types
- Comprehensive data types for all API operations
- Built-in error handling
- Support for all APIFRAME endpoints

## Installation

### Using Nix (recommended)

```bash
# Enter development shell
nix develop

# Build the project
cabal build
```

### Using Cabal directly

```bash
cabal update
cabal build
```

## Usage

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Web.Apiframe.Client
import System.Environment (getEnv)
import Data.Text (pack)

main :: IO ()
main = do
  -- Get API key from environment
  apiKey <- pack <$> getEnv "APIFRAME_API_KEY"
  
  -- Create client
  client <- mkApiframeClient apiKey
  
  -- Generate an image
  let request = ImagineRequest
        { imaginePrompt = "a beautiful sunset over mountains"
        , imagineAspectRatio = Just AspectRatio16x9
        , imagineProcessMode = Just ProcessModeFast
        , imagineWebhookUrl = Nothing
        , imagineWebhookSecret = Nothing
        }
  
  result <- imagine client request
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right response -> putStrLn $ "Task ID: " ++ show (taskId response)
```

## API Documentation

Full API documentation is available at https://docs.apiframe.pro

## Development

```bash
# Run tests
cabal test

# Start REPL
cabal repl

# Run example
APIFRAME_API_KEY=your_key_here cabal run apiframe-example
```

## License

MIT License - see LICENSE file for details
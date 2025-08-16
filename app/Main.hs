module Main (main) where

import qualified Data.Text as T
import System.Environment (lookupEnv)
import Web.Apiframe.Client
import Web.Apiframe.Types

main :: IO ()
main = do
  -- Get API key from environment variable
  apiKeyMaybe <- lookupEnv "APIFRAME_API_KEY"
  apiKey <- case apiKeyMaybe of
    Just key -> return (T.pack key)
    Nothing -> error "APIFRAME_API_KEY environment variable not set"
  
  -- Create client
  client <- mkApiframeClient apiKey
  
  -- Example: Generate an image
  putStrLn "Generating image..."
  let imagineReq = ImagineRequest
        { imaginePrompt = Prompt "a nice day in the desert with my dog"
        , imagineAspectRatio = Just (AspectRatio 3 2)
        , imagineProcessMode = Just ProcessModeFast
        , imagineWebhookUrl = Nothing
        , imagineWebhookSecret = Nothing
        }
  
  result <- imagine client imagineReq
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right TaskResponse{..} -> do
      putStrLn $ "Task ID: " ++ T.unpack (unTaskId taskId)
      case taskErrors of
        Nothing -> putStrLn "Task submitted successfully!"
        Just errors -> putStrLn $ "Errors: " ++ show errors
  
  -- Example: Check account status
  putStrLn "\nChecking account status..."
  accountResult <- account client
  case accountResult of
    Left err -> putStrLn $ "Error: " ++ show err
    Right AccountResponse{..} -> do
      putStrLn $ "Email: " ++ T.unpack (unEmail accountEmail)
      putStrLn $ "Credits: " ++ show accountCredits
      putStrLn $ "Plan: " ++ T.unpack (unPlan accountPlan)
      putStrLn $ "Total images: " ++ show accountTotalImages
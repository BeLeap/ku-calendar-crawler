module Main where

import Network.HTTP

main :: IO ()
main = do
  result <- simpleHTTP (getRequest "https://example.com")
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right response -> putStrLn $ "Response Code:" ++ show (rspCode response)

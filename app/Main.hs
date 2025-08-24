{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Simple

main :: IO ()
main = do
  response <- httpLBS "https://example.com"
  putStrLn $ "ResCode: " ++ show (getResponseStatusCode response)

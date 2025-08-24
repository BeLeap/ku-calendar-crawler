{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Simple

main :: IO ()
main = do
  response <- httpLBS "https://registrar.korea.ac.kr/eduinfo/affairs/schedule.do?cYear=2025&hakGi=2&srCategoryId1=1613"
  putStrLn $ "ResCode: " ++ show (getResponseStatusCode response)

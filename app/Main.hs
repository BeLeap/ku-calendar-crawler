{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Simple (parseRequest, setRequestMethod, setRequestQueryString, setRequestHeaders, httpSink)
import Text.HTML.DOM (sinkDoc)
import Text.XML.Cursor (element, fromDocument, ($//), (&//))

main :: IO ()
main = do
  request' <- parseRequest "https://registrar.korea.ac.kr/eduinfo/affairs/schedule.do"
  let request
        = setRequestMethod "GET"
        $ setRequestQueryString [("cYear", Just "2025"), ("hakGi", Just "2"), ("srCategoryId1", Just "1613")]
        $ setRequestHeaders [("User-Agent", "curl/8.14.1")]
        $ request'
  document <- httpSink request $ const sinkDoc

  let cursor = fromDocument document

  let rows = cursor $// element "table" &// element "tr"
  let content = map (\row -> (row $// element "th", row $// element "td")) rows

  print content

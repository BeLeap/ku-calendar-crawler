{-# LANGUAGE OverloadedStrings #-}
module Main where

import Text.Pretty.Simple (pPrint)
import Network.HTTP.Simple (parseRequest, setRequestMethod, setRequestQueryString, setRequestHeaders, httpSink)
import Text.HTML.DOM (sinkDoc)
import Text.XML.Cursor
import Data.Text (strip)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.List (mapAccumL)
import Data.Char (isDigit)
import qualified Data.Text as T

data DataRange
  = Single Int | Range Int Int

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

  let rows' = cursor $// element "table" &// element "tr"
  let rows = map (\row -> (
          listToMaybe $ map strip $ row $// element "th" &// element "span" &/ content,
          strip $ head $ row $// attributeIs "class" "des dateInfo" &/ content,
          strip $ head $ row $// attributeIs "class" "des" &// element "div" &/ content
        )) rows'

  let (_, withMonth) = mapAccumL step "" rows
        where
          step acc (mMonth, date, title) = let
              newMonth = fromMaybe acc mMonth
            in (newMonth, (newMonth, date, title))

  let parsed = map (\(month, date, title) -> (month, parseDate $ T.unpack date, title)) withMonth
        where
          readNum xs =
            let
              (ds, rest) = span isDigit xs
            in (read ds, rest)
          skipWeekday xs =
            case dropWhile (/= ')') xs of
              [] -> []
              (_:ys) -> ys
          parseDate input =
            let
              (d1, _) = readNum input
              rest2 = skipWeekday input
            in case rest2 of
              ('~':more) ->
                let
                  (d2, _) = readNum more
                  _ = skipWeekday more
                in Range d1 d2
              _ -> Single d1

  putStrLn ""

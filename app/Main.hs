{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Simple (parseRequest, setRequestMethod, setRequestQueryString, setRequestHeaders, httpSink)
import Text.HTML.DOM (sinkDoc)
import Text.XML.Cursor
import Data.Text (strip)
import Data.Maybe (listToMaybe, fromMaybe)
import Data.List (mapAccumL)
import Data.Char (isDigit)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.ICalendar
import Data.Default (def)
import Data.Time.Clock
import Data.Time (fromGregorian)
import Data.Version
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as BL
import Text.Pretty.Simple

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

  let parsed = map (\(month, date, title) -> (parseMonth month :: Int, parseDate $ T.unpack date, title)) withMonth
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
          parseMonth input = read (T.unpack (T.dropEnd 1 input))

  now <- getCurrentTime

  let vEvents = map (
                      \(month, date, title) ->
                        let
                          (start, end) =
                            case date of
                              Single s -> (Just DTStartDate { dtStartDateValue = Date (fromGregorian 2025 month s), dtStartOther = def }, Nothing)
                              Range s e -> (Just (DTStartDate (Date (fromGregorian 2025 month s)) def), Just (DTEndDate (Date (fromGregorian 2025 month e)) def))
                        in ((TL.fromStrict title, Nothing), VEvent {
                          veDTStamp = DTStamp now def,
                          veUID = UID (TL.fromStrict title) def,
                          veClass = Class Public def,
                          veDTStart = start,
                          veCreated = Nothing,
                          veDescription = Nothing,
                          veGeo = Nothing,
                          veLastMod = Nothing,
                          veLocation = Nothing,
                          veOrganizer = Nothing,
                          vePriority = def,
                          veSeq = def,
                          veStatus = Nothing,
                          veSummary = Just (Summary (TL.pack $ T.unpack title ++ "|ku-calendar-crawler@beleap.dev") Nothing Nothing def),
                          veTransp = def,
                          veUrl = Nothing,
                          veRecurId = Nothing,
                          veRRule = S.empty,
                          veDTEndDuration = fmap Left end,
                          veAttach = S.empty,
                          veAttendee = S.empty,
                          veCategories = S.empty,
                          veComment = S.empty,
                          veContact = S.empty,
                          veExDate = S.empty,
                          veRStatus = S.empty,
                          veRelated = S.empty,
                          veResources = S.empty,
                          veRDate = S.empty,
                          veAlarms = S.empty,
                          veOther = S.empty
                        })
                    ) parsed

  let vCal = VCalendar {
    vcProdId = ProdId (TL.pack "-//BeLeap//ku-calendar-crawler//EN") def,
    vcVersion = MinMaxICalVersion
    (makeVersion [2,0])
    (makeVersion [2,0])
    def,
    vcScale = Scale "Gregorian" def,
    vcMethod = Nothing,
    vcOther = S.empty,
    vcTimeZones = M.empty,
    vcEvents = M.fromList vEvents,
    vcTodos = M.empty,
    vcJournals = M.empty,
    vcFreeBusys = M.empty,
    vcOtherComps = S.empty
  }

  let icsBytes = printICalendar def vCal

  BL.writeFile "result.ics" icsBytes

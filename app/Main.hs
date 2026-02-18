{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Simple (parseRequest, setRequestMethod, setRequestQueryString, setRequestHeaders, httpSink)
import Network.HTTP.Client (Request)
import Text.HTML.DOM (sinkDoc)
import Text.XML.Cursor
import Data.Text (strip)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
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
import qualified Text.XML
import Data.Text.Encoding (encodeUtf8)

data DataRange
  = Single Int | Range Int Int
instance Show DataRange where
  show (Single d) = "Single-" ++ show d
  show (Range s e) = "Double-" ++ show s ++ "-" ++ show e

buildRequest :: String -> String -> IO Request
buildRequest year term = do
  request' <- parseRequest "https://registrar.korea.ac.kr/eduinfo/affairs/schedule.do"
  pure
    $ setRequestMethod "GET"
    $ setRequestQueryString
      [ ("cYear", Just $ encodeUtf8 $ T.pack year)
      , ("hakGi", Just $ encodeUtf8 $ T.pack term)
      , ("srCategoryId1", Just "1613")
      ]
    $ setRequestHeaders [("User-Agent", "curl/8.14.1")]
    $ request'

crawl :: String -> String -> IO Text.XML.Document
crawl year term = do
  request <- buildRequest year term
  httpSink request $ const sinkDoc

readLeadingDigits :: String -> Maybe Int
readLeadingDigits xs =
  case span isDigit xs of
    ("", _) -> Nothing
    (ds, _) -> Just (read ds)

skipWeekday :: String -> String
skipWeekday xs =
  case dropWhile (/= ')') xs of
    [] -> []
    (_:ys) -> ys

parseDateRange :: T.Text -> Maybe DataRange
parseDateRange input = do
  start <- readLeadingDigits raw
  let rest = skipWeekday raw
  case rest of
    ('~':more) ->
      fmap (Range start) (readLeadingDigits more)
    _ ->
      pure (Single start)
  where
    raw = T.unpack input

parseMonth :: T.Text -> Maybe Int
parseMonth input = readLeadingDigits (T.unpack (T.dropEnd 1 input))

extractRow :: Cursor -> Maybe (T.Text, T.Text, T.Text)
extractRow row = do
  date <- listToMaybe $ row $// attributeIs "class" "des dateInfo" &/ content
  title <- listToMaybe $ row $// attributeIs "class" "des" &// element "div" &/ content
  let month = fromMaybe "" $ listToMaybe $ map strip $ row $// element "th" &// element "span" &/ content
  pure (month, strip date, strip title)

parseDocument :: Text.XML.Document -> [(Int, DataRange, T.Text)]
parseDocument document =
  let
    cursor = fromDocument document

    rows' = cursor $// element "table" &// element "tr"
    rows = mapMaybe extractRow rows'

    (_, withMonth) = mapAccumL step "" rows
          where
            step acc (month, date, title) = let
                newMonth = if T.null month then acc else month
              in (newMonth, (newMonth, date, title))
    toEvent (month, date, title) = do
      m <- parseMonth month
      d <- parseDateRange date
      pure (m, d, title)
  in
  mapMaybe toEvent withMonth

makeDate :: Integer -> Int -> Int -> Date
makeDate year month day = Date (fromGregorian year month day)

eventTimeRange :: Integer -> Int -> DataRange -> (Maybe DTStart, Maybe Date)
eventTimeRange year month dateRange =
  case dateRange of
    Single s -> (Just DTStartDate { dtStartDateValue = makeDate year month s, dtStartOther = def }, Nothing)
    Range s e ->
      ( Just (DTStartDate (makeDate year month s) def)
      , Just (makeDate year month e)
      )

generateIcalEvents :: UTCTime -> Integer -> [(Int, DataRange, T.Text)] -> [((TL.LazyText, Maybe (Either Date DateTime)), VEvent)]
generateIcalEvents now year = map (
        \(month, date, title) ->
          let
            (start, endDate) = eventTimeRange year month date
            end = fmap (\d -> DTEndDate d def) endDate
          in ((TL.fromStrict title, Nothing), VEvent {
            veDTStamp = DTStamp now def,
            veUID = UID (TL.pack $ T.unpack title ++ "|" ++ show year ++ "|" ++ show month  ++ "|" ++ show date ++ "|ku-calendar-crawler@beleap.dev") def,
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
            veSummary = Just (Summary (TL.fromStrict title) Nothing Nothing def),
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
      )

main :: IO ()
main = do
  let targets = [("2025", "1"), ("2025", "2")]
  documents <- mapM (uncurry crawl) targets
  let calInfos = map parseDocument documents
  let infos = zip targets calInfos
  now <- getCurrentTime
  let events = concatMap (\((year, _), info) -> generateIcalEvents now (read year) info) infos

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
    vcEvents = M.fromList events,
    vcTodos = M.empty,
    vcJournals = M.empty,
    vcFreeBusys = M.empty,
    vcOtherComps = S.empty
  }

  let icsBytes = printICalendar def vCal

  BL.writeFile "result.ics" icsBytes

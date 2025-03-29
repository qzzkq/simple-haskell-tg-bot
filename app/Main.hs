{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.Text (Text, pack, unpack)
import Data.Time.Clock (diffUTCTime, UTCTime)
import Data.Time.LocalTime
  ( getZonedTime, zonedTimeToUTC,
    ZonedTime(..), zonedTimeZone, LocalTime(..), TimeOfDay(..) )
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Time.Calendar (Day, toGregorian)
import Data.Int (Int64)
import Servant.Client (ClientEnv, runClientM)
import Text.Printf (printf)
import Text.Read (readMaybe)
import Text.Regex.Posix ((=~))
import Telegram.Bot.Simple
  ( BotApp(..), Eff, replyText, startBot_, (<#) )
import Telegram.Bot.Simple.UpdateParser (plainText)
import Telegram.Bot.API (Update)
import Telegram.Bot.API.Types 
  ( ChatId(..),
    Message(messageChat),
    SomeChatId(..) )
import qualified Telegram.Bot.API as Telegram
  ( Token(..),
    defaultTelegramClientEnv,
    SendMessageRequest(..),
    sendMessage )

import Data.Aeson (toJSON, Value(..))
import qualified Data.Aeson.KeyMap as KM
import Data.Scientific (toBoundedInteger)

data Model = Model 
  { telegramEnv :: ClientEnv
  }

data Action
  = RemindRequest ChatId LocalTime Text
  | StartCommand
  | UnknownCommand
  deriving (Show, Eq)

timePattern :: String
timePattern = "^remind at ([0-9]{4}-[0-9]{2}-[0-9]{2}) (2[0-3]|1[0-9]|0[0-9]):([0-5][0-9]) (.*)$"

parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%Y-%m-%d"

extractChatId :: Update -> Maybe ChatId
extractChatId upd = case toJSON upd of
  Object obj -> do
    msgVal <- KM.lookup "message" obj
    case msgVal of
      Object msgObj -> do
        chatVal <- KM.lookup "chat" msgObj
        case chatVal of
          Object chatObj -> do 
            idVal <- KM.lookup "id" chatObj
            case idVal of
              Number n -> ChatId <$> (fromIntegral <$> toBoundedInteger @Int64 n)
              _        -> Nothing
          _ -> Nothing
      _ -> Nothing
  _ -> Nothing

handleUpdate :: Update -> Model -> Maybe Action
handleUpdate upd _ =
  case runReaderT plainText upd of
    Just txt -> case extractChatId upd of
      Just cid ->
        let input = unpack txt
            matches :: (String, String, String, [String])
            matches = input =~ timePattern
        in case matches of
            (_, _, _, [dateStr, hrStr, minStr, bodyStr]) ->
              case (parseDate dateStr, readMaybe hrStr, readMaybe minStr) of
                (Just day, Just hour, Just minu) ->
                  let timeOfDay = TimeOfDay hour minu 0
                      localTime = LocalTime day timeOfDay
                  in Just (RemindRequest cid localTime (pack bodyStr))
                _ -> Just UnknownCommand
            _ -> Just UnknownCommand
      Nothing -> Nothing
    Nothing -> Nothing

formatLocalTime :: LocalTime -> Text
formatLocalTime (LocalTime day (TimeOfDay h m _)) = 
  pack $ printf "%04d-%02d-%02d %02d:%02d" year month dayOfMonth h m
  where
    (year, month, dayOfMonth) = toGregorian day

main :: IO ()
main = do
  let botToken = Telegram.Token "WRITE_YOUR_TOKEN_HERE"
  env <- Telegram.defaultTelegramClientEnv botToken

  let botApp = BotApp
        { botInitialModel = Model env
        , botAction = handleUpdate
        , botHandler = handleAction
        , botJobs = []
        }

  startBot_ botApp env

handleAction :: Action -> Model -> Eff Action Model
handleAction action model@(Model env) = case action of
  RemindRequest cid localTime reminderText ->
    model <# do
      nowZoned <- liftIO getZonedTime
      let tz = zonedTimeZone nowZoned
          targetZoned = ZonedTime localTime tz
          targetUTC = zonedTimeToUTC targetZoned
          currentUTC = zonedTimeToUTC nowZoned


      if targetUTC <= currentUTC
        then replyText "Указанное время в прошлом. Укажите другое."
        else do
          _ <- liftIO $ forkIO $ do
            let diffSeconds = diffUTCTime targetUTC currentUTC
                microsTotal = ceiling (realToFrac diffSeconds * 1000000)
                maxDelay = maxBound :: Int

                recWait remaining
                  | remaining <= 0 = return ()
                  | remaining > fromIntegral maxDelay = do
                      threadDelay maxDelay
                      recWait (remaining - fromIntegral maxDelay)
                  | otherwise = threadDelay (fromIntegral remaining)

            recWait microsTotal

            let req = Telegram.SendMessageRequest
                  { Telegram.sendMessageChatId = SomeChatId cid
                  , Telegram.sendMessageText = reminderText
                  , Telegram.sendMessageParseMode = Nothing
                  , Telegram.sendMessageEntities = Nothing
                  , Telegram.sendMessageDisableNotification = Nothing
                  , Telegram.sendMessageReplyToMessageId = Nothing
                  , Telegram.sendMessageReplyMarkup = Nothing
                  , Telegram.sendMessageBusinessConnectionId = Nothing
                  , Telegram.sendMessageMessageThreadId = Nothing
                  , Telegram.sendMessageProtectContent = Nothing
                  , Telegram.sendMessageLinkPreviewOptions = Nothing
                  , Telegram.sendMessageMessageEffectId = Nothing
                  , Telegram.sendMessageReplyParameters = Nothing
                  }

            res <- runClientM (Telegram.sendMessage req) env
            case res of
              Left err -> putStrLn $ "Ошибка отправки сообщения: " ++ show err
              Right _ -> return ()

          replyText $ "Хорошо, я напомню вам "
                    <> formatLocalTime localTime
                    <> ": "
                    <> reminderText

  UnknownCommand ->
    model <# do
      replyText "Неверный формат. Используйте : \n\"remind at YYYY-MM-DD HH:MM your message\""
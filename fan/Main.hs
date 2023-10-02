{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}

module Main where

import Control.Applicative (Alternative (..))
import Control.Exception (bracket, bracket_)
import Control.Monad
import Data.ByteString qualified as BS
import Data.Char (toLower)
import Data.Foldable
import Data.Functor (($>))
import Data.Maybe
import Data.Word
import Foreign (Bits (..), finalizeForeignPtr)
import Options.Applicative (header, progDesc)
import Options.Applicative.Types (readerAsk)
import Options.Generic
import System.HID qualified as HID
import System.IO
import Unsafe.Coerce (unsafeCoerce)

main :: IO ()
main = withHID do
  opts <-
    getRecordWith
      ( header
          "Modify Lian-Li fan controllers"
          <> progDesc
            ( unlines
                [ "WARNING: the commands this program sends are not fully understood,"
                , "while it hasn't bricked my fan controller (yet), there are no guarantees that"
                , "it won't put a fan controller into an unrecoverable state"
                ]
            )
      )
      mempty
  case unHelpful (setSpeed opts) of
    Just (Manual rpm) | rpm > 100 -> error "Requested fan speed too high, maximum is 100"
    _ -> pure ()
  defaultDevices <- findFans
  when (unHelpful (listDevices opts)) $ for_ defaultDevices \d -> putStrLn (HID.devProductString d <> ": " <> HID.devPath d)
  let paths = case unHelpful (device opts) of
        [] -> HID.devPath <$> defaultDevices
        ps -> ps
  for_ paths \p -> withPathDevice p \h -> do
    for_ (unHelpful (setColor opts)) (setColor' h)
    for_ (unHelpful (setSpeed opts)) (setRPM h)

----------------------------------------------------------------
-- Setup
----------------------------------------------------------------

findFans :: IO [HID.DeviceInfo]
findFans = do
  ds <- HID.enumerate 0x0cf2 0xa105
  case filter ((== 1) . HID.devInterfaceNumber) ds of
    [] -> hPutStrLn stderr "No fan controllers found" $> []
    ds' -> pure ds'

----------------------------------------------------------------
-- Modification
----------------------------------------------------------------

setColor' :: HID.Device -> ColorSync -> IO ()
setColor' d s =
  let syncByte = case s of
        Sync -> 1
        DoNotSync -> 0
   in void $ HID.writeOutputReport d (BS.pack [0xe0, 0x10, 0x61, syncByte, 0, 0, 0])

data RPM = PWM | Manual Word8
  deriving (Show)

setRPM :: HID.Device -> RPM -> IO ()
setRPM d rpm =
  for_ [0 .. 3] \i -> do
    let channelByte =
          ( case rpm of
              PWM -> 0x11
              Manual _ -> 0x10
          )
            `shiftL` i
    _ <- HID.writeOutputReport d (BS.pack [0xe0, 0x10, 0x62, channelByte])
    case rpm of
      PWM -> pure ()
      Manual p ->
        let s = round @Double $ (realToFrac p * 17.5 + 250) / 20
         in void $
              HID.writeOutputReport d $
                BS.pack [0xe0, 0x20 + fromIntegral i, 0x00, s]

----------------------------------------------------------------
-- Options
----------------------------------------------------------------

data ColorSync = DoNotSync | Sync

data Options = Options
  { setSpeed
      :: Maybe RPM
        <?> "Set the target fan speed to either the PWM input or a specific speed between 0% and 100%"
  , setColor
      :: Maybe ColorSync
        <?> "Set the color to either sync to the RGB header or to specific inner and outer colors"
  , device
      :: [String]
        <?> "A specific device to use, for example 5-10:1.1"
  , listDevices
      :: Bool
        <?> "Print the paths of all recognized pumps"
  }
  deriving (Generic)

instance ParseRecord Options where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

instance ParseField ColorSync where
  readField =
    Sync <$ (guard . (== "sync") . fmap toLower =<< readerAsk)
      <|> DoNotSync <$ (guard . (== "nosync") . fmap toLower =<< readerAsk)
  metavar _ = "sync|nosync"

instance ParseField RPM where
  readField =
    PWM <$ (guard . (== "pwm") . fmap toLower =<< readerAsk)
      <|> Manual <$> readField
  metavar _ = "pwm|INT"

----------------------------------------------------------------
-- Utils
----------------------------------------------------------------

withHID :: IO c -> IO c
withHID = bracket_ HID.init HID.exit

withPathDevice :: String -> (HID.Device -> IO c) -> IO c
withPathDevice p = bracket (fromMaybe (error "couldn't open device") <$> HID.pathDevice p) closeDevice

closeDevice :: HID.Device -> IO ()
closeDevice = finalizeForeignPtr . unsafeCoerce

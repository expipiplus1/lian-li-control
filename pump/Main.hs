{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant <&>" #-}

module Main where

import Control.Applicative (Alternative (..))
import Control.Concurrent.Async
import Control.Exception (bracket_)
import Control.Exception.Base (bracket)
import Control.Monad
import Data.ByteString (ByteString)
import Data.ByteString qualified as B (length)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Char (toLower)
import Data.Foldable
import Data.Functor (($>), (<&>))
import Data.Maybe
import Data.Storable.Endian (peekBE)
import Data.Word
import Foreign (Storable (..), castPtr, plusPtr)
import Foreign.ForeignPtr (finalizeForeignPtr)
import Numeric (readHex)
import Options.Applicative (header, progDesc)
import Options.Applicative.Types (readerAsk)
import Options.Generic
import System.HID qualified as HID
import System.IO
import Text.ParserCombinators.ReadP
import Text.Show.Pretty
import Unsafe.Coerce (unsafeCoerce)

main :: IO ()
main = withHID $ do
  opts <-
    getRecordWith
      ( header
          "Query and modify Lian-Li AIO coolers"
          <> progDesc
            ( unlines
                [ "WARNING: the commands this program sends are not fully understood,"
                , "while it hasn't bricked my pump (yet), there are no guarantees that"
                , "it won't put a pump into an unrecoverable state"
                ]
            )
      )
      mempty
  case unHelpful (setSpeed opts) of
    Just (Manual rpm) | rpm < 2200 -> error "Requested pump speed too low, minimum is 2200"
    Just (Manual rpm) | rpm > 4200 -> error "Requested pump speed too high, maximum is 4200"
    _ -> pure ()
  defaultDevices <- findPumps
  when (unHelpful (listDevices opts)) $ for_ defaultDevices \d -> putStrLn (HID.devProductString d <> ": " <> HID.devPath d)
  let paths = case unHelpful (device opts) of
        [] -> HID.devPath <$> defaultDevices
        ps -> ps
  for_ paths \p -> withPathDevice p \h -> do
    when (unHelpful (getSpeed opts)) do
      s <- getStatus h
      print (statusRPM s)
    for_ (unHelpful (setSpeed opts)) (setRPM h)
    for_ (unHelpful (setColor opts)) (setColor' h)

----------------------------------------------------------------
-- Setup
----------------------------------------------------------------

findPumps :: IO [HID.DeviceInfo]
findPumps = do
  ds <- HID.enumerate 0x0416 0x7371
  -- The second interface is the one which has the speed and color controls
  case filter ((== 2) . HID.devInterfaceNumber) ds of
    [] -> hPutStrLn stderr "No pumps found" $> []
    ds' -> pure ds'

----------------------------------------------------------------
-- Query
----------------------------------------------------------------

data PumpStatus = PumpStatus
  { status0 :: Word8
  , status1 :: Word8
  , status2 :: Word8
  , status3 :: Word8
  , status4 :: Word8
  , status5 :: Word8
  , statusRPM :: Word16
  , status6 :: Word8
  , status7 :: Word8
  , status8 :: Word8
  , status9 :: Word8
  }
  deriving (Show)

instance Storable PumpStatus where
  peek p =
    PumpStatus
      <$> peekByteOff p 2
      <*> peekByteOff p 3
      <*> peekByteOff p 4
      <*> peekByteOff p 5
      <*> peekByteOff p 6
      <*> peekByteOff p 7
      <*> peekBE (plusPtr p 8)
      <*> peekByteOff p 10
      <*> peekByteOff p 11
      <*> peekByteOff p 12
      <*> peekByteOff p 13
  poke = error "unimplemented status poke"
  sizeOf _ = 64
  alignment _ = 2

getStatus :: HID.Device -> IO PumpStatus
getStatus h =
  withAsync (HID.readInputReport h) \r -> do
    -- Request the status
    _ <- HID.writeOutputReport h (pack64 [0x01, 0x81])
    wait r >>= \case
      Nothing -> error "failed to read"
      Just b | B.length b < sizeOf @PumpStatus undefined -> error "read too short"
      Just b -> BS.unsafeUseAsCString b (peek @PumpStatus . castPtr)

----------------------------------------------------------------
-- Modification
----------------------------------------------------------------

colorPackets :: Colors -> [ByteString]
colorPackets c =
  let syncByte =
        case c of
          InnerOuter _ _ -> 0x00
          SyncToMB -> 0x01
      (Color iR iG iB, Color oR oG oB) = case c of
        InnerOuter i o -> (i, o)
        SyncToMB -> (Color 0xff 0xff 0xff, Color 0xff 0xff 0xff)
   in pack64
        <$> [ [0x01]
                <> [0x83, 0x00, 0x00, 0x00, 0x13, 0x02, 0x03, 0x04]
                <> ([0x00] <> [iR, iG, iB] <> [oR, oG, oB] <> [0x00])
                <> [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, syncByte]
            , [0x01]
                <> [0x85, 0x00, 0x00, 0x00, 0x14, 0x03, 0x04, 0x00]
                <> [0x00, 0xff, 0xaa, 0x00, 0x00, 0x00, 0x00, 0x00]
                <> [0x00, 0x00, 0x00, 0x00, 0x00, 0x00, syncByte, 0x00]
                <> [0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
            ]

data Color = Color Word8 Word8 Word8
data Colors = SyncToMB | InnerOuter Color Color

setColor' :: HID.Device -> Colors -> IO ()
setColor' d c = do
  traverse_ (HID.writeOutputReport d) (colorPackets c)

data RPM = PWM | Manual Word16
  deriving (Show)

setRPM :: HID.Device -> RPM -> IO ()
setRPM d rpm = do
  let syncByte = case rpm of
        PWM -> 0x01
        Manual _ -> 0x00
  let speed = case rpm of
        PWM -> 0x64
        Manual r ->
          let r'
                | r < 2200 = 2200
                | r > 4200 = 4200
                | otherwise = fromIntegral r
           in round @Double ((r' - 2200) / (4200 - 2200)) * (100 - 29) + 29
  pPrint speed
  _ <-
    HID.writeOutputReport
      d
      ( pack64
          ( [0x01]
              <> [0x8b, 0x00, 0x00, 0x00, 0x02, syncByte, 0x00, 0x00]
          )
      )
  _ <-
    HID.writeOutputReport
      d
      ( pack64
          ( [0x01]
              <> [0x8a, 0x00, 0x00, 0x00, 0x02, syncByte, speed, 0x00]
          )
      )
  pure ()

pack64 :: [Word8] -> ByteString
pack64 = BS.pack . take 64 . (<> repeat 0)

----------------------------------------------------------------
-- Options
----------------------------------------------------------------

data Options = Options
  { getSpeed
      :: Bool
        <?> "Print the current speed of the pump"
  , setSpeed
      :: Maybe RPM
        <?> "Set the target pump speed to either the PWM input or a specific speed between 2200 and 4200 RPM"
  , setColor
      :: Maybe Colors
        <?> "Set the color to either sync to the RGB header or to specific inner and outer colors"
  , device
      :: [String]
        <?> "A specific device to use, for example 5-10:1.2"
  , listDevices
      :: Bool
        <?> "Print the paths of all recognized pumps"
  }
  deriving (Generic)

instance ParseRecord Options where
  parseRecord = parseRecordWithModifiers lispCaseModifiers

instance ParseField Colors where
  readField =
    SyncToMB <$ (guard . (== "sync") . fmap toLower =<< readerAsk)
      <|> ( readField
              <&> \(ColorPair a b) -> InnerOuter a b
          )
  metavar _ = "sync|rrggbb:rrggbb"

data ColorPair = ColorPair Color Color
instance ParseField ColorPair
instance Read ColorPair where
  readsPrec _ = readP_to_S do
    let c =
          count 2 get <&> readHex >>= \case
            [(c', "")] -> pure c'
            _ -> fail "Couldn't parse color"
    c1 <- Color <$> c <*> c <*> c
    _ <- char ':'
    c2 <- Color <$> c <*> c <*> c
    pure $ ColorPair c1 c2

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

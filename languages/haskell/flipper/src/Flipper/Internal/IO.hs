module Flipper.Internal.IO where

import Foreign.Marshal.Utils

import Data.Word

data DigitalPin = IO1
                | IO2
                | IO3
                | IO4
                | IO5
                | IO6
                | IO7
                | IO8
                | IO9
                | IO10
                | IO11
                | IO12
                | IO13
                | IO14
                | IO15
                | IO16
                deriving (Eq, Ord, Show)

data AnalogPin = A0
               | A1
               | A2
               | A3
               | A4
               | A5
               | A6
               | A7
               | A8
               deriving (Eq, Ord, Show)

data Direction = Input
               | Output
               deriving (Eq, Ord, Show)

digPinCode :: DigitalPin -> Word8
digPinCode IO1  = 1
digPinCode IO2  = 2
digPinCode IO3  = 3
digPinCode IO4  = 4
digPinCode IO5  = 5
digPinCode IO6  = 6
digPinCode IO7  = 7
digPinCode IO8  = 8
digPinCode IO9  = 9
digPinCode IO10 = 10
digPinCode IO11 = 11
digPinCode IO12 = 12
digPinCode IO13 = 13
digPinCode IO14 = 14
digPinCode IO15 = 15
digPinCode IO16 = 16

anPinCode :: AnalogPin -> Word8
anPinCode A0 = 17
anPinCode A1 = 18
anPinCode A2 = 19
anPinCode A3 = 20
anPinCode A4 = 21
anPinCode A5 = 22
anPinCode A6 = 23
anPinCode A7 = 24
anPinCode A8 = 25

directionCode :: Direction -> Word8
directionCode Input  = 0
directionCode Output = 1

digitalDirection :: DigitalPin -> Direction -> IO ()
digitalDirection p d = c_io_set_direction (digPinCode p) (directionCode d)

analogDirection :: AnalogPin -> Direction -> IO ()
analogDirection p d = c_io_set_direction (anPinCode p) (directionCode d)

digitalWrite :: DigitalPin -> Bool -> IO ()
digitalWrite p v = c_io_write (digPinCode p) (fromBool v)

--digitalPinPulse :: DigitalPin -> Word16 -> IO ()
--digitalPinPulse p d = c_digital_pulse (digPinCode p) d

analogWrite :: AnalogPin -> Word16 -> IO ()
analogWrite p v = c_io_write (anPinCode p) v

foreign import ccall safe "flipper/io.h io_set_direction"
    c_io_set_direction :: Word8 -> Word8 -> IO ()

foreign import ccall safe "flipper/io.h io_write"
    c_io_write :: Word8 -> Word16 -> IO ()

foreign import ccall safe "flipper/io.h io_read"
    c_io_read :: Word8 -> IO Word16

--foreign import ccall safe "flipper/io.h digital_pulse"
--    c_digital_pulse :: Word8 -> Word16 -> IO ()

module Flipper.Console.Parsers where

import Control.Applicative

import Data.Word

import Flipper

import Flipper.Console.Action

import Flipper.IO
import Flipper.LED
import Flipper.USART

import Flipper.Internal.FS

import Text.Megaparsec
import Text.Megaparsec.Lexer
import Text.Megaparsec.String

-- | One or more whitespace characters.
spaces :: Parser ()
spaces = skipSome spaceChar

parseConsoleAction :: Parser ConsoleAction
parseConsoleAction = choice [ try parseFlash
                            , try parseInstall
                            , try parseLaunch
                            , try parseReset
                            , try parseSuspend
                            , try parseEngage
                            , try parseFormat
                            , ConsoleCall <$> parseCall
                            ]

parseFlash :: Parser ConsoleAction
parseFlash = Flash <$> (string' "flash" *> spaces *> parseEscString)

parseInstall :: Parser ConsoleAction
parseInstall = Install <$> (string' "install" *> spaces *> parseBundleID) <*> parseEscString

parseLaunch :: Parser ConsoleAction
parseLaunch = Launch <$> (string' "launch" *> spaces *> parseEscString)

parseReset :: Parser ConsoleAction
parseReset = string' "reset" *> pure Reset

parseSuspend :: Parser ConsoleAction
parseSuspend = string' "suspend" *> pure Suspend

parseEngage :: Parser ConsoleAction
parseEngage = string' "engage" *> pure Engage

parseFormat :: Parser ConsoleAction
parseFormat = string' "format" *> pure Format

parseCall :: Parser Call
parseCall = choice [ ButtonCall <$> try parseButtonAction
                   , ConfigCall <$> try parseConfigAction
                   , IOCall <$> try parseIOAction
                   , LEDCall <$> try parseLEDAction
                   , NVMCall <$> try parseNVMAction
                   , SPICall <$> try parseSPICall
                   , USARTCall <$> try parseUSARTCall
                   , USBCall <$> try parseUSBCall
                   ]

parseNVMAction :: Parser NVMAction
parseNVMAction = string' "nvm" *> spaces *> choice [ try parseNVMEnable
                                                   , try parseNVMDisable
                                                   , try parseNVMReset
                                                   , try parseNVMFormat
                                                   , try parseNVMAlloc
                                                   , try parseNVMRead
                                                   , try parseNVMWrite
                                                   ]

parseButtonAction :: Parser ButtonAction
parseButtonAction = string' "button" *> spaces *> choice [ try parseButtonRead
                                                         ]

parseConfigAction :: Parser ConfigAction
parseConfigAction = string' "config"  *> spaces *> choice [ try parseConfigRead
                                                          , try parseConfigWrite
                                                          ]

parseIOAction :: Parser IOAction
parseIOAction = string' "io" *> spaces *> choice [ try parseIODigitalDirection
                                                 , try parseIODigitalRead
                                                 , try parseIODigitalWrite
                                                 , try parseIOAnalogDirection
                                                 , try parseIOAnalogRead
                                                 , try parseIOAnalogWrite
                                                 ]

parseLEDAction :: Parser LEDAction
parseLEDAction = string' "led" *> spaces *> choice [ try parseLEDsetRGB
                                                   ]

parseSPICall :: Parser SPIAction
parseSPICall = string' "spi" *> spaces *> choice [ try parseSPIEnable
                                                 , try parseSPIDisable
                                                 , try parseSPIRead
                                                 , try parseSPIWrite
                                                 ]

parseUSARTCall :: Parser USARTAction
parseUSARTCall = bus <**> (spaces *> action)
    where bus    = choice [ try (string' "usart0" *> pure USART0)
                          , try (string' "usart1" *> pure USART1)
                          , try (string' "dbgu" *> pure DBGU)
                          ]
          action = choice [ try parseUSARTEnable
                          , try parseUSARTDisable
                          , try parseUSARTRead
                          , try parseUSARTWrite
                          ]

parseUSBCall :: Parser USBAction
parseUSBCall = string' "usb" *> spaces *> choice [ try parseUSBEnable
                                                 , try parseUSBDisable
                                                 , try parseUSBRead
                                                 , try parseUSBWrite
                                                 ]

parseNVMEnable :: Parser NVMAction
parseNVMEnable = string' "enable" *> pure NVMEnable

parseNVMDisable :: Parser NVMAction
parseNVMDisable = string' "disable" *> pure NVMDisable

parseNVMReset :: Parser NVMAction
parseNVMReset = string' "reset" *> pure NVMReset

parseNVMFormat :: Parser NVMAction
parseNVMFormat = string' "format" *> pure NVMFormat

parseNVMAlloc :: Parser NVMAction
parseNVMAlloc = NVMAlloc <$> (string' "alloc" *> spaces *> parseAllocSize)

parseNVMRead :: Parser NVMAction
parseNVMRead = NVMRead <$> (string' "read" *> spaces *> parseFSHandle)

parseNVMWrite :: Parser NVMAction
parseNVMWrite = NVMWrite <$> (string' "write" *> spaces *> parseFSHandle)
                           <*> (spaces *> parseEscString)

parseButtonRead :: Parser ButtonAction
parseButtonRead = string "read" *> pure ButtonRead

parseConfigRead :: Parser ConfigAction
parseConfigRead = ConfigRead <$> (string "read" *> spaces *> parseWord8)

parseConfigWrite :: Parser ConfigAction
parseConfigWrite = ConfigWrite <$> (string "write" *> spaces *> parseWord8)
                               <*> (spaces *> parseWord16)

parseIODigitalDirection :: Parser IOAction
parseIODigitalDirection = IODigitalDirection <$> (string' "direction" *> spaces *> parseDigitalPin)
                                             <*> (spaces *> parseDirection)

parseIODigitalRead :: Parser IOAction
parseIODigitalRead = IODigitalRead <$> (string' "read" *> spaces *> parseDigitalPin)

parseIODigitalWrite :: Parser IOAction
parseIODigitalWrite = IODigitalWrite <$> (string' "write" *> spaces *> parseDigitalPin)
                                     <*> (spaces *> parseBool)

parseIOAnalogDirection :: Parser IOAction
parseIOAnalogDirection = IOAnalogDirection <$> (string' "direction" *> spaces *> parseAnalogPin)
                                           <*> (spaces *> parseDirection)

parseIOAnalogRead :: Parser IOAction
parseIOAnalogRead = IOAnalogRead <$> (string' "read" *> spaces *> parseAnalogPin)

parseIOAnalogWrite :: Parser IOAction
parseIOAnalogWrite = IOAnalogWrite <$> (string' "write" *> spaces *> parseAnalogPin)
                                   <*> (spaces *> parseWord16)

parseLEDsetRGB :: Parser LEDAction
parseLEDsetRGB = LEDSetRGB <$> (string' "set" *> spaces *> parseRGB)

parseSPIEnable :: Parser SPIAction
parseSPIEnable = string' "enable" *> pure SPIEnable

parseSPIDisable :: Parser SPIAction
parseSPIDisable = string' "disable" *> pure SPIDisable

parseSPIRead :: Parser SPIAction
parseSPIRead = string' "read" *> pure SPIRead

parseSPIWrite :: Parser SPIAction
parseSPIWrite = SPIWrite <$> (string' "write" *> spaces *> parseEscString)

parseUSARTEnable :: Parser (USART -> USARTAction)
parseUSARTEnable = string' "enable" *> pure USARTEnable

parseUSARTDisable :: Parser (USART -> USARTAction)
parseUSARTDisable = string' "disable" *> pure USARTDisable

parseUSARTRead :: Parser (USART -> USARTAction)
parseUSARTRead = string' "read" *> pure USARTRead

parseUSARTWrite :: Parser (USART -> USARTAction)
parseUSARTWrite = (\s -> (\b -> USARTWrite b s)) <$> (string' "write" *> spaces *> parseEscString)

parseUSBEnable :: Parser USBAction
parseUSBEnable = string' "enable" *> pure USBEnable

parseUSBDisable :: Parser USBAction
parseUSBDisable = string' "disable" *> pure USBDisable

parseUSBRead :: Parser USBAction
parseUSBRead = string' "read" *> pure USBRead

parseUSBWrite :: Parser USBAction
parseUSBWrite = USBWrite <$> (string' "write" *> spaces *> parseEscString)

parseDigitalPin :: Parser DigitalPin
parseDigitalPin = choice [ try (string' "10" *> pure IO10)
                         , try (string' "11" *> pure IO11)
                         , try (string' "12" *> pure IO12)
                         , try (string' "13" *> pure IO13)
                         , try (string' "14" *> pure IO14)
                         , try (string' "15" *> pure IO15)
                         , try (string' "16" *> pure IO16)
                         , try (string' "1" *> pure IO1)
                         , try (string' "2" *> pure IO2)
                         , try (string' "3" *> pure IO3)
                         , try (string' "4" *> pure IO4)
                         , try (string' "5" *> pure IO5)
                         , try (string' "6" *> pure IO6)
                         , try (string' "7" *> pure IO7)
                         , try (string' "8" *> pure IO8)
                         , try (string' "9" *> pure IO9)
                         ]

parseAnalogPin :: Parser AnalogPin
parseAnalogPin = choice [ try (string' "a1" *> pure A1)
                        , try (string' "a2" *> pure A2)
                        , try (string' "a3" *> pure A3)
                        , try (string' "a4" *> pure A4)
                        , try (string' "a5" *> pure A5)
                        , try (string' "a6" *> pure A6)
                        , try (string' "a7" *> pure A7)
                        , try (string' "a8" *> pure A8)
                        ]

parseDirection :: Parser Direction
parseDirection = choice [ try (string' "input" *> pure Input)
                        , try (string' "output" *> pure Output)
                        , try (string' "in" *> pure Input)
                        , try (string' "out" *> pure Output)
                        ]

parseRGB :: Parser RGB
parseRGB = RGB <$> parseWord8
               <*> (spaces *> parseWord8)
               <*> (spaces *> parseWord8)

parseFSHandle :: Parser FSHandle
parseFSHandle = (FSHandle . fromIntegral) <$> parseIntegerLit

parseBundleID :: Parser BundleID
parseBundleID = BundleID <$> sepBy1 (someTill alphaNumChar (string' ".")) (string' ".")

parseEscString :: Parser String
parseEscString = string' "\"" *> esc
    where esc = do c <- anyChar
                   case c of '\\' -> anyChar >>= (\c' -> (c':) <$> esc)
                             '"'  -> pure []
                             _    -> (c:) <$> esc

parseBool :: Parser Bool
parseBool = choice [ try (string' "0" *> pure False)
                   , try (string' "false" *> pure False)
                   , try (string' "f" *> pure False)
                   , try (string' "1" *> pure True)
                   , try (string' "true" *> pure True)
                   , try (string' "t" *> pure True)
                   ]

parseAllocSize :: Parser Int
parseAllocSize = fromIntegral <$> parseIntegerLit

parseWord8 :: Parser Word8
parseWord8 = fromIntegral <$> parseIntegerLit

parseWord16 :: Parser Word16
parseWord16 = fromIntegral <$> parseIntegerLit

parseIntegerLit :: Parser Integer
parseIntegerLit = choice [ try integer
                         , try (string "0x" *> hexadecimal)
                         ]

parseEndpoint :: Parser Endpoint
parseEndpoint = choice [ try parseUSBEndpoint
                       , try parseFVMEndpoint
                       , parseNetworkEndpoint
                       ]

parseUSBEndpoint :: Parser Endpoint
parseUSBEndpoint = string' "usb" *> option (USB Nothing)
                                    ((USB . Just) <$> (string' ":" *> some alphaNumChar))

parseNetworkEndpoint :: Parser Endpoint
parseNetworkEndpoint = Network <$> (some (alphaNumChar <|> char '.'))

parseFVMEndpoint :: Parser Endpoint
parseFVMEndpoint = string' "fvm" *> pure FVM

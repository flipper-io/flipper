module Flipper.Console.Parsers where

import Control.Applicative

import Data.Word

import Flipper

import Flipper.Console.Action

import Flipper.GPIO
import Flipper.LED

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
                            , try parseFormat
                            , ConsoleCall <$> parseCall
                            ]

parseFlash :: Parser ConsoleAction
parseFlash = Flash <$> (string' "flash" *> spaces *> parseEscString)

parseInstall :: Parser ConsoleAction
parseInstall = Install <$> (string' "install" *> spaces *> parseModuleID) <*> parseEscString

parseLaunch :: Parser ConsoleAction
parseLaunch = Launch <$> (string' "launch" *> spaces *> parseEscString)

parseReset :: Parser ConsoleAction
parseReset = string' "reset" *> pure Reset

parseSuspend :: Parser ConsoleAction
parseSuspend = string' "suspend" *> pure Suspend

parseFormat :: Parser ConsoleAction
parseFormat = string' "format" *> pure Format

parseCall :: Parser Call
parseCall = choice [ ButtonCall <$> try parseButtonAction
                   , FSCall <$> parseFSAction
                   , GPIOCall <$> parseGPIOAction
                   , LEDCall <$> parseLEDAction
                   , SPICall <$> parseSPIAction
                   , UARTCall <$> parseUARTAction
                   ]

parseButtonAction :: Parser ButtonAction
parseButtonAction = string' "button" *> spaces *> choice [ try parseButtonRead
                                                         ]

parseFSAction :: Parser FSAction
parseFSAction = string "fs" *> spaces *> choice [ try parseFSCreateFromString
                                                , try parseFSRemove
                                                , try parseFSRename
                                                ]

parseGPIOAction :: Parser GPIOAction
parseGPIOAction = string' "gpio" *> spaces *> choice [ try parseGPIODigitalDirection
                                                     , try parseGPIODigitalRead
                                                     , try parseGPIODigitalWrite
                                                     , try parseGPIOAnalogDirection
                                                     , try parseGPIOAnalogRead
                                                     , try parseGPIOAnalogWrite
                                                     ]

parseLEDAction :: Parser LEDAction
parseLEDAction = string' "led" *> spaces *> choice [ try parseLEDsetRGB
                                                   ]

parseSPIAction :: Parser SPIAction
parseSPIAction = string' "spi" *> spaces *> choice [ try parseSPIEnable
                                                 , try parseSPIDisable
                                                 , try parseSPIRead
                                                 , try parseSPIWriteFromString
                                                 ]

parseUARTAction :: Parser UARTAction
parseUARTAction = string' "uart" *> spaces *> choice [ try parseUARTEnable
                                                     , try parseUARTDisable
                                                     , try parseUARTRead
                                                     , try parseUARTWriteFromString
                                                     ]

parseButtonRead :: Parser ButtonAction
parseButtonRead = string "read" *> pure ButtonRead

parseFSCreateFromString :: Parser FSAction
parseFSCreateFromString = FSCreateFromString <$> (string "create" *> spaces *> parseEscString)
                                             <*> (spaces *> parseEscString)

parseFSRemove :: Parser FSAction
parseFSRemove = FSRemove <$> (string' "remove" *> spaces *> parseEscString)

parseFSRename :: Parser FSAction
parseFSRename = FSRename <$> (string' "rename" *> spaces *> parseEscString)
                         <*> (spaces *> parseEscString)

parseGPIODigitalDirection :: Parser GPIOAction
parseGPIODigitalDirection = GPIODigitalDirection <$> (string' "direction" *> spaces *> parseDigitalPin)
                                                 <*> (spaces *> parseDirection)

parseGPIODigitalRead :: Parser GPIOAction
parseGPIODigitalRead = GPIODigitalRead <$> (string' "read" *> spaces *> parseDigitalPin)

parseGPIODigitalWrite :: Parser GPIOAction
parseGPIODigitalWrite = GPIODigitalWrite <$> (string' "write" *> spaces *> parseDigitalPin)
                                         <*> (spaces *> parseBool)

parseGPIOAnalogDirection :: Parser GPIOAction
parseGPIOAnalogDirection = GPIOAnalogDirection <$> (string' "direction" *> spaces *> parseAnalogPin)
                                               <*> (spaces *> parseDirection)

parseGPIOAnalogRead :: Parser GPIOAction
parseGPIOAnalogRead = GPIOAnalogRead <$> (string' "read" *> spaces *> parseAnalogPin)

parseGPIOAnalogWrite :: Parser GPIOAction
parseGPIOAnalogWrite = GPIOAnalogWrite <$> (string' "write" *> spaces *> parseAnalogPin)
                                       <*> (spaces *> parseWord16)

parseLEDsetRGB :: Parser LEDAction
parseLEDsetRGB = LEDSetRGB <$> (string' "set" *> spaces *> parseRGB)

parseSPIEnable :: Parser SPIAction
parseSPIEnable = string' "enable" *> pure SPIEnable

parseSPIDisable :: Parser SPIAction
parseSPIDisable = string' "disable" *> pure SPIDisable

parseSPIRead :: Parser SPIAction
parseSPIRead = string' "read" *> pure SPIRead

parseSPIWriteFromString :: Parser SPIAction
parseSPIWriteFromString = SPIWriteFromString <$> (string' "write" *> spaces *> parseEscString)

parseUARTEnable :: Parser UARTAction
parseUARTEnable = string' "enable" *> pure UARTEnable

parseUARTDisable :: Parser UARTAction
parseUARTDisable = string' "disable" *> pure UARTDisable

parseUARTRead :: Parser UARTAction
parseUARTRead = string' "read" *> pure UARTRead

parseUARTWriteFromString :: Parser UARTAction
parseUARTWriteFromString = UARTWriteFromString <$> (string' "write" *> spaces *> parseEscString)

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

parseModuleID :: Parser ModuleID
parseModuleID = ModuleID <$> sepBy1 (someTill alphaNumChar (string' ".")) (string' ".")

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
parseNetworkEndpoint = Network <$> (some alphaNumChar) <*> (some (alphaNumChar <|> char '.'))

parseFVMEndpoint :: Parser Endpoint
parseFVMEndpoint = string' "fvm" *> pure FVM

{-|
Module      : Flipper.Console.Options
Description : Command line option parser.
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides the REPL command parsers.
-}

module Flipper.Console.Parsers (
    parseConsoleAction
  , parseDigitalPin
  , parseAnalogPin
  , parseDirection
  , parseEndpoint
  , parseModuleID
  , parseBool
  , parseWord8
  , parseWord16
  , parseWord32
  ) where

import Data.Word

import Flipper.Console.Action

import Flipper

import Flipper.GPIO
import Flipper.LED

import Text.Megaparsec
import Text.Megaparsec.Lexer
import Text.Megaparsec.String

-- | One or more whitespace characters.
spaces :: Parser ()
spaces = skipSome spaceChar

-- | Top-level 'ConsoleAction' parser.
parseConsoleAction :: Parser ConsoleAction
parseConsoleAction = choice [ try parseFlash
                            , try parseInstall
                            , try parseLaunch
                            , try parseReset
                            , try parseSuspend
                            , try parseFormat
                            , ConsoleCall <$> parseCall
                            ]

-- | Firmware flash command parser.
parseFlash :: Parser ConsoleAction
parseFlash = Flash <$> (string' "flash" *> spaces *> parseEscString)

-- | Module install command parser.
parseInstall :: Parser ConsoleAction
parseInstall = Install <$> (string' "install" *> spaces *> parseModuleID)
                       <*> parseEscString

-- | Module launch command parser.
parseLaunch :: Parser ConsoleAction
parseLaunch = Launch <$> (string' "launch" *> spaces *> parseEscString)

-- | Device reset command parser.
parseReset :: Parser ConsoleAction
parseReset = string' "reset" *> pure Reset

-- | Device suspend command parser.
parseSuspend :: Parser ConsoleAction
parseSuspend = string' "suspend" *> pure Suspend

-- | Device format command parser.
parseFormat :: Parser ConsoleAction
parseFormat = string' "format" *> pure Format

-- | Standard module call parser.
parseCall :: Parser Call
parseCall = choice [ ADCCall <$> try parseADCAction
                   , ButtonCall <$> try parseButtonAction
                   , DACCall <$> try parseDACAction
                   , FSCall <$> try parseFSAction
                   , GPIOCall <$> try parseGPIOAction
                   , I2CCall <$> try parseI2CAction
                   , LEDCall <$> try parseLEDAction
                   , PWMCall <$> try parsePWMAction
                   , RTCCall <$> try parseRTCAction
                   , SPICall <$> try parseSPIAction
                   , SWDCall <$> try parseSWDAction
                   , TempCall <$> try parseTempAction
                   , TimerCall <$> try parseTimerAction
                   , UART0Call <$> try parseUART0Action
                   , USARTCall <$> try parseUSARTAction
                   , USBCall <$> try parseUSBAction
                   , WDTCall <$> try parseWDTAction
                   ]

-- | ADC command parser.
parseADCAction :: Parser ADCAction
parseADCAction = string' "adc" *> spaces *> choice [ try parseADCConfigure
                                                   ]

-- | Button command parser.
parseButtonAction :: Parser ButtonAction
parseButtonAction = string' "button" *> spaces *> choice [ try parseButtonConfigure
                                                         , try parseButtonRead
                                                         ]

-- | DAC command parser.
parseDACAction :: Parser DACAction
parseDACAction = string' "dac" *> spaces *> choice [ try parseDACConfigure
                                                   ]

-- | File system command parser.
parseFSAction :: Parser FSAction
parseFSAction = string' "fs" *> spaces *> choice [ try parseFSConfigure
                                                 , try parseFSCreate
                                                 , try parseFSDelete
                                                 , try parseFSSize
                                                 , try parseFSOpen
                                                 , try parseFSPushString
                                                 , try parseFSPullString
                                                 , try parseFSClose
                                                 ]

-- | GPIO command parser.
parseGPIOAction :: Parser GPIOAction
parseGPIOAction = string' "gpio" *> spaces *> choice gpios
    where gpios = [ try parseGPIOConfigure
                  , try parseGPIODigitalDirection
                  , try parseGPIODigitalRead
                  , try parseGPIODigitalWrite
                  , try parseGPIOAnalogDirection
                  , try parseGPIOAnalogRead
                  , try parseGPIOAnalogWrite
                  ]

-- | I2C command parser.
parseI2CAction :: Parser I2CAction
parseI2CAction = string' "i2c" *> spaces *> choice [ try parseI2CConfigure
                                                   ]

-- | LED command parser.
parseLEDAction :: Parser LEDAction
parseLEDAction = string' "led" *> spaces *> choice [ try parseLEDConfigure
                                                   , try parseLEDsetRGB
                                                   ]

-- | PWM command parser.
parsePWMAction :: Parser PWMAction
parsePWMAction = string "pwm" *> spaces *> choice [ try parsePWMConfigure
                                                  ]

-- | RTC command parser.
parseRTCAction :: Parser RTCAction
parseRTCAction = string "rtc" *> spaces *> choice [ try parseRTCConfigure
                                                  ]

-- | SPI command parser.
parseSPIAction :: Parser SPIAction
parseSPIAction = string' "spi" *> spaces *> choice [ try parseSPIConfigure
                                                   , try parseSPIEnable
                                                   , try parseSPIDisable
                                                   , try parseSPIRead
                                                   , try parseSPIWriteFromString
                                                   , try parseSPIWriteFromFile
                                                   ]

-- | SWD command parser.
parseSWDAction :: Parser SWDAction
parseSWDAction = string' "swd" *> spaces *> choice [ try parseSWDConfigure
                                                   ]

-- | Temperature command parser.
parseTempAction :: Parser TempAction
parseTempAction = string' "temp" *> spaces *> choice [ try parseTempConfigure
                                                     ]

-- | Timer command parser.
parseTimerAction :: Parser TimerAction
parseTimerAction = string' "timer" *> spaces *> choice [ try parseTimerConfigure
                                                     ]

-- | UART0 command parser.
parseUART0Action :: Parser UART0Action
parseUART0Action = string' "uart0" *> spaces *> choice uart0s
    where uart0s = [ try parseUART0Configure
                   , try parseUART0Enable
                   , try parseUART0Disable
                   , try parseUART0Read
                   , try parseUART0WriteFromString
                   , try parseUART0WriteFromFile
                   ]

-- | USART command parser.
parseUSARTAction :: Parser USARTAction
parseUSARTAction = string' "usart" *> spaces *> choice usarts
    where usarts = [ try parseUSARTConfigure
                   , try parseUSARTEnable
                   , try parseUSARTDisable
                   , try parseUSARTRead
                   , try parseUSARTWriteFromString
                   , try parseUSARTWriteFromFile
                   ]

-- | USB command parser.
parseUSBAction :: Parser USBAction
parseUSBAction = string' "usb" *> spaces *> choice [ try parseUSBConfigure
                                                   ]

-- | WDT command parser.
parseWDTAction :: Parser WDTAction
parseWDTAction = string' "wdt" *> spaces *> choice [ try parseWDTConfigure
                                                   , try parseWDTFire
                                                   ]

-- | ADC configure command parser.
parseADCConfigure :: Parser ADCAction
parseADCConfigure = string' "configure" *> pure ADCConfigure

-- | Button configure command parser.
parseButtonConfigure :: Parser ButtonAction
parseButtonConfigure = string' "configure" *> pure ButtonConfigure

-- | Button read command parser.
parseButtonRead :: Parser ButtonAction
parseButtonRead = string' "read" *> pure ButtonRead

-- | DAC configure command parser.
parseDACConfigure :: Parser DACAction
parseDACConfigure = string' "configure" *> pure DACConfigure

-- | File system configure command parser.
parseFSConfigure :: Parser FSAction
parseFSConfigure = string' "configure" *> pure FSConfigure

-- | File system create command parser.
parseFSCreate :: Parser FSAction
parseFSCreate = FSCreate <$> (string' "create" *> spaces *> parseEscString)

-- | File system delete command parser.
parseFSDelete :: Parser FSAction
parseFSDelete = FSDelete <$> (string' "remove" *> spaces *> parseEscString)

-- | File system size query command parser.
parseFSSize :: Parser FSAction
parseFSSize = FSSize <$> (string' "size" *> spaces *> parseEscString)

-- | File system open command parser.
parseFSOpen :: Parser FSAction
parseFSOpen = FSOpen <$> (string' "open" *> spaces *> parseEscString)
                     <*> (spaces *> parseWord32)

-- | File system null-terminated string write command parser.
parseFSPushString :: Parser FSAction
parseFSPushString = FSPushString <$> ( string' "push"
                                       *> spaces
                                       *> parseEscString
                                     )

-- | File system null-terminated string read command parser.
parseFSPullString :: Parser FSAction
parseFSPullString = string' "pull" *> pure FSPullString

-- | File system close command parser.
parseFSClose :: Parser FSAction
parseFSClose = string' "close" *> pure FSClose

-- | GPIO configure command parser.
parseGPIOConfigure :: Parser GPIOAction
parseGPIOConfigure = string' "configure" *> pure GPIOConfigure

-- | GPIO digital pin direction set command parser.
parseGPIODigitalDirection :: Parser GPIOAction
parseGPIODigitalDirection = GPIODigitalDirection <$> ( string' "direction"
                                                       *> spaces
                                                       *> parseDigitalPin
                                                     )
                                                 <*> (spaces *> parseDirection)

-- | GPIO digital pin read command parser.
parseGPIODigitalRead :: Parser GPIOAction
parseGPIODigitalRead = GPIODigitalRead <$> ( string' "read"
                                             *> spaces
                                             *> parseDigitalPin
                                           )

-- | GPIO digital pin write command parser.
parseGPIODigitalWrite :: Parser GPIOAction
parseGPIODigitalWrite = GPIODigitalWrite <$> ( string' "write"
                                               *> spaces
                                               *> parseDigitalPin
                                             )
                                         <*> (spaces *> parseBool)

-- | GPIO analog pin direction set command parser.
parseGPIOAnalogDirection :: Parser GPIOAction
parseGPIOAnalogDirection = GPIOAnalogDirection <$> ( string' "direction"
                                                     *> spaces
                                                     *> parseAnalogPin
                                                   )
                                               <*> (spaces *> parseDirection)

-- | GPIO analog pin read command parser.
parseGPIOAnalogRead :: Parser GPIOAction
parseGPIOAnalogRead = GPIOAnalogRead <$> ( string' "read"
                                           *> spaces
                                           *> parseAnalogPin
                                         )

-- | GPIO analog pin write command parser.
parseGPIOAnalogWrite :: Parser GPIOAction
parseGPIOAnalogWrite = GPIOAnalogWrite <$> ( string' "write"
                                             *> spaces
                                             *> parseAnalogPin
                                           )
                                       <*> (spaces *> parseWord16)

-- | I2C configure command parser.
parseI2CConfigure :: Parser I2CAction
parseI2CConfigure = string' "configure" *> pure I2CConfigure

-- | LED configure command parser.
parseLEDConfigure :: Parser LEDAction
parseLEDConfigure = string' "configure" *> pure LEDConfigure

-- | LED RGB command parser.
parseLEDsetRGB :: Parser LEDAction
parseLEDsetRGB = LEDSetRGB <$> (string' "set" *> spaces *> parseRGB)

-- | PWM configure command parser.
parsePWMConfigure :: Parser PWMAction
parsePWMConfigure = string' "configure" *> pure PWMConfigure

-- | RTC configure command parser.
parseRTCConfigure :: Parser RTCAction
parseRTCConfigure = string' "configure" *> pure RTCConfigure

-- | SPI configure command parser.
parseSPIConfigure :: Parser SPIAction
parseSPIConfigure = string' "configure" *> pure SPIConfigure

-- | SPI enable command parser.
parseSPIEnable :: Parser SPIAction
parseSPIEnable = string' "enable" *> pure SPIEnable

-- | SPI disable command parser.
parseSPIDisable :: Parser SPIAction
parseSPIDisable = string' "disable" *> pure SPIDisable

-- | SPI read command parser.
parseSPIRead :: Parser SPIAction
parseSPIRead = string' "read" *> pure SPIRead

-- | SPI null-terminated string write command parser.
parseSPIWriteFromString :: Parser SPIAction
parseSPIWriteFromString = SPIWriteFromString <$> ( string' "write"
                                                   *> spaces
                                                   *> parseEscString
                                                 )

-- | SPI file write command parser.
parseSPIWriteFromFile :: Parser SPIAction
parseSPIWriteFromFile = SPIWriteFromFile <$> ( string' "writefile"
                                               *> spaces
                                               *> parseEscString
                                             )

-- | SWD configure command parser.
parseSWDConfigure :: Parser SWDAction
parseSWDConfigure = string' "configure" *> pure SWDConfigure

-- | Temperature configure command parser.
parseTempConfigure :: Parser TempAction
parseTempConfigure = string' "configure" *> pure TempConfigure

-- | Timer configure command parser.
parseTimerConfigure :: Parser TimerAction
parseTimerConfigure = string' "configure" *> pure TimerConfigure

-- | UART0 configure command parser.
parseUART0Configure :: Parser UART0Action
parseUART0Configure = string' "configure" *> pure UART0Configure

-- | UART0 enable command parser.
parseUART0Enable :: Parser UART0Action
parseUART0Enable = string' "enable" *> pure UART0Enable

-- | UART0 disable command parser.
parseUART0Disable :: Parser UART0Action
parseUART0Disable = string' "disable" *> pure UART0Disable

-- | UART0 read command parser.
parseUART0Read :: Parser UART0Action
parseUART0Read = string' "read" *> pure UART0Read

-- | UART0 null-terminated string write command parser.
parseUART0WriteFromString :: Parser UART0Action
parseUART0WriteFromString = UART0WriteFromString <$> ( string' "write"
                                                     *> spaces
                                                     *> parseEscString
                                                   )

-- | UART0 file write command parser.
parseUART0WriteFromFile :: Parser UART0Action
parseUART0WriteFromFile = UART0WriteFromFile <$> ( string' "writefile"
                                                 *> spaces
                                                 *> parseEscString
                                                 )

-- | USART configure command parser.
parseUSARTConfigure :: Parser USARTAction
parseUSARTConfigure = string' "configure" *> pure USARTConfigure

-- | USART enable command parser.
parseUSARTEnable :: Parser USARTAction
parseUSARTEnable = string' "enable" *> pure USARTEnable

-- | USART disable command parser.
parseUSARTDisable :: Parser USARTAction
parseUSARTDisable = string' "disable" *> pure USARTDisable

-- | USART read command parser.
parseUSARTRead :: Parser USARTAction
parseUSARTRead = string' "read" *> pure USARTRead

-- | USART null-terminated string write command parser.
parseUSARTWriteFromString :: Parser USARTAction
parseUSARTWriteFromString = USARTWriteFromString <$> ( string' "write"
                                                     *> spaces
                                                     *> parseEscString
                                                   )

-- | USART file write command parser.
parseUSARTWriteFromFile :: Parser USARTAction
parseUSARTWriteFromFile = USARTWriteFromFile <$> ( string' "writefile"
                                                 *> spaces
                                                 *> parseEscString
                                                 )

-- | USB configure command parser.
parseUSBConfigure :: Parser USBAction
parseUSBConfigure = string' "configure" *> pure USBConfigure

-- | WDT configure command parser.
parseWDTConfigure :: Parser WDTAction
parseWDTConfigure = string' "configure" *> pure WDTConfigure

-- | WDT fire command parser.
parseWDTFire :: Parser WDTAction
parseWDTFire = string' "fire" *> pure WDTFire

-- | Digital pin identifier parser.
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

-- | Analog pin identifier parser.
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

-- | I/O direction parser.
parseDirection :: Parser Direction
parseDirection = choice [ try (string' "input" *> pure Input)
                        , try (string' "output" *> pure Output)
                        , try (string' "in" *> pure Input)
                        , try (string' "out" *> pure Output)
                        ]

-- | RGB parser.
parseRGB :: Parser RGB
parseRGB = RGB <$> parseWord8
               <*> (spaces *> parseWord8)
               <*> (spaces *> parseWord8)

-- | Deprecate this.
parseModuleID :: Parser ModuleID
parseModuleID = ModuleID <$> sepBy1 (someTill alphaNumChar (string' "."))
                                    (string' ".")

-- | Quote-escaped 'String' parser.
parseEscString :: Parser String
parseEscString = string' "\"" *> esc
    where esc = do c <- anyChar
                   case c of '\\' -> anyChar >>= (\c' -> (c':) <$> esc)
                             '"'  -> pure []
                             _    -> (c:) <$> esc

-- | 'Bool' parser.
parseBool :: Parser Bool
parseBool = choice [ try (string' "0" *> pure False)
                   , try (string' "false" *> pure False)
                   , try (string' "f" *> pure False)
                   , try (string' "1" *> pure True)
                   , try (string' "true" *> pure True)
                   , try (string' "t" *> pure True)
                   ]

-- | 'Int' parser.
--parseAllocSize :: Parser Int
--parseAllocSize = fromIntegral <$> parseIntegerLit

-- | 'Word8' parser.
parseWord8 :: Parser Word8
parseWord8 = fromIntegral <$> parseIntegerLit

-- | 'Word16' parser.
parseWord16 :: Parser Word16
parseWord16 = fromIntegral <$> parseIntegerLit

-- | 'Word32' parser.
parseWord32 :: Parser Word32
parseWord32 = fromIntegral <$> parseIntegerLit

-- | Integer literal parser. Works with unqualified decimal interers or
--   hexidecimal numbers prefixed with @0x@.
parseIntegerLit :: Parser Integer
parseIntegerLit = choice [ try integer
                         , try (string' "0x" *> hexadecimal)
                         ]

-- | Device 'Endpoint' parser.
parseEndpoint :: Parser Endpoint
parseEndpoint = choice [ try parseUSBEndpoint
                       , try parseFVMEndpoint
                       , parseNetworkEndpoint
                       ]

-- | USB 'Endpoint' parser.
parseUSBEndpoint :: Parser Endpoint
parseUSBEndpoint = string' "usb" *> option (USB Nothing)
                                    ((USB . Just) <$> ( string' ":"
                                                        *> some alphaNumChar)
                                                      )

-- | Network 'Endpoint' parser.
parseNetworkEndpoint :: Parser Endpoint
parseNetworkEndpoint = Network <$> some alphaNumChar
                               <*> some (alphaNumChar <|> char '.')

-- | FVM 'Endpoint' parser.
parseFVMEndpoint :: Parser Endpoint
parseFVMEndpoint = string' "fvm" *> pure FVM

{-|
Module      : Flipper.Console.Options
Description : Command line option parser.
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

This module provides the command line option parsers.
-}

module Flipper.Console.Options (
    Options(..)
  , options
  ) where

import Control.Applicative

import Data.Word

import Flipper
import Flipper.GPIO (DigitalPin(..), AnalogPin(..), Direction(..))
import Flipper.LED

import Flipper.Console.Action
import Flipper.Console.Parsers

import Options.Applicative hiding (action)

import qualified Text.Megaparsec        as M
import qualified Text.Megaparsec.String as M

-- | Run-time options.
data Options = Options {
    -- | Device endpoint.
    optEndpoint :: Endpoint
    -- | One-off action provided on the command line.
  , optOneoff   :: Maybe ConsoleAction
  } --deriving ( Eq
    --         , Ord
    --         , Show
    --         )

-- | Top-level option parser.
options :: Parser Options
options = Options <$> endpoint <*> optional action

-- | Endpoint parser.
endpoint :: Parser Endpoint
endpoint = option (readParser parseEndpoint) opt
    where opt = mconcat [ long "endpoint"
                        , short 'e'
                        , metavar "ENDPOINT"
                        , help "The endpoint via which Flipper\
                               \ will be attached."
                        , value (USB Nothing)
                        ]
-- | Run a megaparsec parser as a command line option parser.
readParser :: M.Parser a -> ReadM a
readParser p = eitherReader (showLeft . M.runParser p "")
    where showLeft (Right x) = Right x
          showLeft (Left e)  = Left (show e)

-- | Top-level action parser.
action :: Parser ConsoleAction
action = hsubparser (mconcat builtins <> mconcat calls)
    where builtins = [ command "flash" flash
                     , command "install" install
                     , command "launch" launch
                     , command "reset" reset
                     , command "suspend" suspend
                     , command "format" format
                     ]
          calls = [ command "adc" ((ConsoleCall . ADCCall) <$> adc)
                  , command "button" ((ConsoleCall . ButtonCall) <$> button)
                  , command "dac" ((ConsoleCall . DACCall) <$> dac)
                  , command "fs" ((ConsoleCall . FSCall) <$> fs)
                  , command "gpio" ((ConsoleCall . GPIOCall) <$> gpio)
                  , command "i2c" ((ConsoleCall . I2CCall) <$> i2c)
                  , command "led" ((ConsoleCall . LEDCall) <$> led)
                  , command "pwm" ((ConsoleCall . PWMCall) <$> pwm)
                  , command "rtc" ((ConsoleCall . RTCCall) <$> rtc)
                  , command "spi" ((ConsoleCall . SPICall) <$> spi)
                  , command "swd" ((ConsoleCall . SWDCall) <$> swd)
                  , command "temp" ((ConsoleCall . TempCall) <$> temp)
                  , command "timer" ((ConsoleCall . TimerCall) <$> timer)
                  , command "uart0" ((ConsoleCall . UART0Call) <$> uart0)
                  , command "usart" ((ConsoleCall . USARTCall) <$> usart)
                  , command "usb" ((ConsoleCall . USBCall) <$> usb)
                  , command "wdt" ((ConsoleCall . WDTCall) <$> wdt)
                  ]

-- | Firmware flash action parser.
flash :: ParserInfo ConsoleAction
flash = info (Flash <$> flashP) flashI
    where flashI = mconcat [ fullDesc
                           , progDesc "Flash firmware to the device."
                           ]
          flashP = strArgument $ mconcat [ metavar "FIRMWARE"
                                         , help "Firmware file image."
                                         ]

-- | Module install action parser.
install :: ParserInfo ConsoleAction
install = info (Install <$> moduleID <*> installP) installI
    where installI = mconcat [ fullDesc
                             , progDesc "Install a module on the device."
                             ]
          installP = strArgument $ mconcat [ metavar "MODULE_FILE"
                                           , help "Module file image."
                                           ]

-- | Module launch acttion parser.
launch :: ParserInfo ConsoleAction
launch = info (Launch <$> launchP) launchI
    where launchI = mconcat [ fullDesc
                            , progDesc "Launch a module on the device."
                            ]
          launchP = strArgument $ mconcat [ metavar "MODULE"
                                          , help "Module name."
                                          ]

-- | Device reset acttion parser.
reset :: ParserInfo ConsoleAction
reset = info (pure Reset) resetI
    where resetI = mconcat [ fullDesc
                           , progDesc "Reset the device."
                           ]

-- | Device suspend action parser.
suspend :: ParserInfo ConsoleAction
suspend = info (pure Suspend) suspendI
    where suspendI = mconcat [ fullDesc
                             , progDesc "Suspend the device."
                             ]

-- | Device format action parser.
format :: ParserInfo ConsoleAction
format = info (pure Format) formatI
    where formatI = mconcat [ fullDesc
                            , progDesc "Format the flash memory on the device."
                            ]

-- | ADC action parser.
adc :: ParserInfo ADCAction
adc = info adcP adcI
    where adcP = hsubparser $ mconcat [ adcConfigure
                                      ]
          adcI = mconcat [ fullDesc
                         , progDesc "Read the device's ADC."
                         ]

-- | Button action parser.
button :: ParserInfo ButtonAction
button = info buttonP buttonI
    where buttonP = hsubparser $ mconcat [ buttonConfigure
                                         , buttonRead
                                         ]
          buttonI = mconcat [ fullDesc
                            , progDesc "Read the device's button state."
                            ]

-- | DAC action parser.
dac :: ParserInfo DACAction
dac = info dacP dacI
    where dacP = hsubparser $ mconcat [ dacConfigure
                                      ]
          dacI = mconcat [ fullDesc
                         , progDesc "Manipulate the device's DAC."
                         ]

-- | File system action parser.
fs :: ParserInfo FSAction
fs = info fsP fsI
    where fsP = hsubparser $ mconcat [ fsConfigure
                                     , fsCreate
                                     , fsDelete
                                     , fsSize
                                     , fsOpen
                                     , fsPushString
                                     , fsPullString
                                     , fsClose
                                     ]
          fsI = mconcat [ fullDesc
                        , progDesc "Interact with the device's file system."
                        ]

-- | GPIO action parser.
gpio :: ParserInfo GPIOAction
gpio = info gpioP gpioI
    where gpioP = hsubparser $ mconcat [ gpioConfigure
                                       , gpioDirection
                                       , gpioRead
                                       , gpioWrite
                                       ]
          gpioI = mconcat  [ fullDesc
                           , progDesc "Interact with the device's GPIO pins."
                           ]

-- | I2C action parser.
i2c :: ParserInfo I2CAction
i2c = info i2cP i2cI
    where i2cP = hsubparser $ mconcat [ i2cConfigure
                                      ]
          i2cI = mconcat [ fullDesc
                         , progDesc "Interact with the device's I2C bus."
                         ]

-- | LED action parser.
led :: ParserInfo LEDAction
led = info ledP ledI
    where ledP = hsubparser $ mconcat [ ledConfigure
                                      , ledRGB
                                      ]
          ledI = mconcat [ fullDesc
                         , progDesc "Set the device's RGB LED state."
                         ]

-- | PWM action parser.
pwm :: ParserInfo PWMAction
pwm = info pwmP pwmI
    where pwmP = hsubparser $ mconcat [ pwmConfigure
                                      ]
          pwmI = mconcat [ fullDesc
                         , progDesc "Manipulate the device's PWM ouput."
                         ]

-- | RTC action parser.
rtc :: ParserInfo RTCAction
rtc = info rtcP rtcI
    where rtcP = hsubparser $ mconcat [ rtcConfigure
                                      ]
          rtcI = mconcat [ fullDesc
                         , progDesc "Manipulate the device's RTC."
                         ]

-- | SPI action parser.
spi :: ParserInfo SPIAction
spi = info spiP spiI
    where spiI = mconcat [ fullDesc
                         , progDesc "Interact with the device's SPI bus."
                         ]
          spiP = hsubparser $ mconcat [ spiConfigure
                                      , spiEnable
                                      , spiDisable
                                      , spiRead
                                      , spiWriteFromString
                                      , spiWriteFromFile
                                      ]

-- | SWD action parser.
swd :: ParserInfo SWDAction
swd = info swdP swdI
    where swdP = hsubparser $ mconcat [ swdConfigure
                                      ]
          swdI = mconcat [ fullDesc
                         , progDesc "Interact with the device's SWD."
                         ]

-- | Temperature action parser.
temp :: ParserInfo TempAction
temp = info tempP tempI
    where tempP = hsubparser $ mconcat [ tempConfigure
                                       ]
          tempI = mconcat [ fullDesc
                          , progDesc "Read the device's thermal hardware."
                          ]

-- | Timer action parser.
timer :: ParserInfo TimerAction
timer = info timerP timerI
    where timerP = hsubparser $ mconcat [ timerConfigure
                                        ]
          timerI = mconcat [ fullDesc
                           , progDesc "Interact with the device's timers."
                           ]

-- | UART0 action parser.
uart0 :: ParserInfo UART0Action
uart0 = info uart0P uart0I
    where uart0I = mconcat [ fullDesc
                          , progDesc "Interact with the device's UART0 bus."
                          ]
          uart0P = hsubparser $ mconcat [ uart0Configure
                                        , uart0Enable
                                        , uart0Disable
                                        , uart0Read
                                        , uart0WriteFromString
                                        , uart0WriteFromFile
                                        ]

-- | USART action parser.
usart :: ParserInfo USARTAction
usart = info usartP usartI
    where usartI = mconcat [ fullDesc
                          , progDesc "Interact with the device's USART bus."
                          ]
          usartP = hsubparser $ mconcat [ usartConfigure
                                        , usartEnable
                                        , usartDisable
                                        , usartRead
                                        , usartWriteFromString
                                        , usartWriteFromFile
                                        ]

-- | USB action parser.
usb :: ParserInfo USBAction
usb = info usbP usbI
    where usbP = hsubparser $ mconcat [ usbConfigure
                                      ]
          usbI = mconcat [ fullDesc
                         , progDesc "Interact with the device's USB."
                         ]

-- | WDT action parser.
wdt :: ParserInfo WDTAction
wdt = info wdtP wdtI
    where wdtP = hsubparser $ mconcat [ wdtConfigure
                                      ]
          wdtI = mconcat [ fullDesc
                         , progDesc "Interact with the device's WDT."
                         ]

-- | ADC configuration parser.
adcConfigure :: Mod CommandFields ADCAction
adcConfigure = command "configure" (info (pure ADCConfigure) configureI)
    where configureI = mconcat [ fullDesc
                               , progDesc "Configure the ADC."
                               ]

-- | Button configuration parser.
buttonConfigure :: Mod CommandFields ButtonAction
buttonConfigure = command "configure" (info (pure ButtonConfigure) configureI)
    where configureI = mconcat [ fullDesc
                               , progDesc "Configure the button."
                               ]

-- | Button read parser.
buttonRead :: Mod CommandFields ButtonAction
buttonRead = command "read" (info (pure ButtonRead) readI)
    where readI = mconcat [ fullDesc
                          , progDesc "Read the device's button state."
                          ]

-- | DAC configuration parser.
dacConfigure :: Mod CommandFields DACAction
dacConfigure = command "configure" (info (pure DACConfigure) configureI)
    where configureI = mconcat [ fullDesc
                               , progDesc "Configure the DAC."
                               ]

-- | File system configuration parser.
fsConfigure :: Mod CommandFields FSAction
fsConfigure = command "configure" (info (pure FSConfigure) configureI)
    where configureI = mconcat [ fullDesc
                               , progDesc "Configure the file system."
                               ]

-- | File creation parser.
fsCreate :: Mod CommandFields FSAction
fsCreate = command "create" (info ( FSCreate
                                    <$> strArgument fnameP
                                  )
                                  createI
                            )
    where fnameP = mconcat [ help "File name to create."
                           , metavar "FILENAME"
                           ]
          createI = mconcat [ fullDesc
                            , progDesc "Create a file on the device from a \
                                       \user-provided string."
                            ]

-- | File deletion parser.
fsDelete :: Mod CommandFields FSAction
fsDelete = command "delete" (info (FSDelete <$> strArgument fnameP) deleteI)
    where fnameP = mconcat [ help "File name to delete."
                           , metavar "FILENAME"
                           ]
          deleteI = mconcat [ fullDesc
                            , progDesc "Delete a file on the device."
                            ]

-- | File size query parser.
fsSize :: Mod CommandFields FSAction
fsSize = command "size" (info (FSSize <$> strArgument fnameP) sizeI)
    where fnameP = mconcat [ help "File name to delete."
                           , metavar "FILENAME"
                           ]
          sizeI = mconcat [ fullDesc
                          , progDesc "Query file size."
                          ]

-- | File open parser.
fsOpen :: Mod CommandFields FSAction
fsOpen = command "open" (info (FSOpen <$> strArgument fnP
                                      <*> word32 offP)
                              openI
                        )
    where fnP = mconcat [ help "File name to open."
                        , metavar "FILENAME"
                        ]
          offP = mconcat [ help "File cursor offset."
                         , metavar "OFFSET"
                         ]
          openI = mconcat [ fullDesc
                          , progDesc "Open a file on the device."
                          ]

-- | File system null-terminated string writing parser.
fsPushString :: Mod CommandFields FSAction
fsPushString = command "push" (info (FSPushString <$> strArgument pP) pushI)
    where pP = mconcat [ help "String payload."
                       , metavar "PAYLOAD"
                       ]
          pushI = mconcat [ fullDesc
                          , progDesc "Write a null-terminated string to the \
                                     \file cursor."
                          ]

-- | File system null-terminated string reading parser.
fsPullString :: Mod CommandFields FSAction
fsPullString = command "pull" (info (pure FSPullString) pullI)
    where pullI = mconcat [ fullDesc
                          , progDesc "Read a null-terminated string from the \
                                     \file cursor."
                          ]

-- | File close parser.
fsClose :: Mod CommandFields FSAction
fsClose = command "close" (info (pure FSClose) closeI)
    where closeI = mconcat [ fullDesc
                           , progDesc "Close the globally open file."
                           ]

-- | GPIO configuration parser.
gpioConfigure :: Mod CommandFields GPIOAction
gpioConfigure = command "configure" (info (pure GPIOConfigure) configureI)
    where configureI = mconcat [ fullDesc
                               , progDesc "Configure the GPIO."
                               ]

-- | GPIO I/O direction set parser.
gpioDirection :: Mod CommandFields GPIOAction
gpioDirection = command "direction" ( info ( digitalDirection
                                             <|> analogDirection
                                           )
                                           directionI
                                    )
    where directionI = mconcat [ fullDesc
                               , progDesc "Set a GPIO pin's I/O direction."
                               ]

-- | Digital pin identifier parser.
digitalPin :: Parser DigitalPin
digitalPin = argument (readParser parseDigitalPin) dpinP
    where dpinP = mconcat [ metavar "DIGITAL_PIN"
                          , help "Digital GPIO pin."
                          ]

-- | Analog pin identifier parser.
analogPin :: Parser AnalogPin
analogPin = argument (readParser parseAnalogPin) apinP
    where apinP = mconcat [ metavar "ANALOG_PIN"
                          , help "Analog GPIO pin."
                          ]

-- | I/O direction parser.
direction :: Parser Direction
direction = argument (readParser parseDirection) dirP
    where dirP = mconcat [ metavar "DIRECTION"
                         , help "GPIO pin I/O direction."
                         ]

-- | Digital pin direction set parser.
digitalDirection :: Parser GPIOAction
digitalDirection = GPIODigitalDirection <$> digitalPin <*> direction

-- | Analog pin direction set parser.
analogDirection :: Parser GPIOAction
analogDirection = GPIOAnalogDirection <$> analogPin <*> direction

-- | GPIO pin read parser.
gpioRead :: Mod CommandFields GPIOAction
gpioRead = command "read" (info (digitalRead <|> analogRead) readI)
    where readI = mconcat [ fullDesc
                          , progDesc "Read a GPIO pin's value."
                          ]

-- | Digital pin read parser.
digitalRead :: Parser GPIOAction
digitalRead = GPIODigitalRead <$> digitalPin

-- | Analog pin read parser.
analogRead :: Parser GPIOAction
analogRead = GPIOAnalogRead <$> analogPin

-- | GPIO pin write parser.
gpioWrite :: Mod CommandFields GPIOAction
gpioWrite = command "write" (info (digitalWrite <|> analogWrite) writeI)
    where writeI = mconcat [ fullDesc
                           , progDesc "Set a GPIO pin's state."
                           ]

-- | Digital pin write parser.
digitalWrite :: Parser GPIOAction
digitalWrite = GPIODigitalWrite <$> digitalPin <*> bool mempty

-- | Analog pin write parser.
analogWrite :: Parser GPIOAction
analogWrite = GPIOAnalogWrite <$> analogPin <*> word16 mempty

-- | I2C configuration parser.
i2cConfigure :: Mod CommandFields I2CAction
i2cConfigure = command "configure" (info (pure I2CConfigure) configureI)
    where configureI = mconcat [ fullDesc
                               , progDesc "Configure the I2C bus."
                               ]

-- | LED configuration parser.
ledConfigure :: Mod CommandFields LEDAction
ledConfigure = command "configure" (info (pure LEDConfigure) configureI)
    where configureI = mconcat [ fullDesc
                               , progDesc "Configure the LED."
                               ]

-- | LED RGB set parser.
ledRGB :: Mod CommandFields LEDAction
ledRGB = command "rgb" (info (LEDSetRGB <$> rgb) rgbI)
    where rgbI = mconcat [ fullDesc
                         , progDesc "Set the device's LED RGB state."
                         ]

-- | PWM configuration parser.
pwmConfigure :: Mod CommandFields PWMAction
pwmConfigure = command "configure" (info (pure PWMConfigure) configureI)
    where configureI = mconcat [ fullDesc
                               , progDesc "Configure the PWM output."
                               ]

-- | RTC configuration parser.
rtcConfigure :: Mod CommandFields RTCAction
rtcConfigure = command "configure" (info (pure RTCConfigure) configureI)
    where configureI = mconcat [ fullDesc
                               , progDesc "Configure the RTC."
                               ]

-- | SPI configuration parser.
spiConfigure :: Mod CommandFields SPIAction
spiConfigure = command "configure" (info (pure SPIConfigure) configureI)
    where configureI = mconcat [ fullDesc
                               , progDesc "Configure the SPI bus."
                               ]

-- | SPI enable parser.
spiEnable :: Mod CommandFields SPIAction
spiEnable = command "enable" (info (pure SPIEnable) enableI)
    where enableI = mconcat [ fullDesc
                            , progDesc "Enable the SPI bus."
                            ]
-- | SPI disable parser.
spiDisable :: Mod CommandFields SPIAction
spiDisable = command "disable" (info (pure SPIDisable) disableI)
    where disableI = mconcat [ fullDesc
                             , progDesc "Disable the SPI bus."
                             ]

-- | SPI null-terminated string reading parser.
spiRead :: Mod CommandFields SPIAction
spiRead = command "read" (info (pure SPIRead) readI)
    where readI = mconcat [ fullDesc
                          , progDesc "Read a null-terminated string from the \
                                     \SPI bus."
                          ]

-- | SPI null-terminated string writing parser.
spiWriteFromString :: Mod CommandFields SPIAction
spiWriteFromString = command "write" ( info ( SPIWriteFromString
                                              <$> strArgument stringP
                                            )
                                            writeI
                                     )
    where stringP = mconcat [ metavar "PAYLOAD"
                            , help "The string to send over SPI."
                            ]
          writeI = mconcat [ fullDesc
                           , progDesc "Write a null-terminated string to the \
                                      \SPI bus."
                           ]

-- | SPI file writing parser.
spiWriteFromFile :: Mod CommandFields SPIAction
spiWriteFromFile = command "writefile" ( info ( SPIWriteFromString
                                                <$> strArgument fileP
                                              )
                                              writeI
                                       )
    where fileP = mconcat [ metavar "FILEPATH"
                          , help "Path to local file."
                          ]
          writeI = mconcat [ fullDesc
                           , progDesc "Write a local file to the SPI bus."
                           ]

-- | SWD configuration parser.
swdConfigure :: Mod CommandFields SWDAction
swdConfigure = command "configure" (info (pure SWDConfigure) configureI)
    where configureI = mconcat [ fullDesc
                               , progDesc "Configure the SWD."
                               ]

-- | Temperature configuration parser.
tempConfigure :: Mod CommandFields TempAction
tempConfigure = command "configure" (info (pure TempConfigure) configureI)
    where configureI = mconcat [ fullDesc
                               , progDesc "Configure the thermal hardware."
                               ]

-- | Timer configuration parser.
timerConfigure :: Mod CommandFields TimerAction
timerConfigure = command "configure" (info (pure TimerConfigure) configureI)
    where configureI = mconcat [ fullDesc
                               , progDesc "Configure the timers."
                               ]

-- | UART0 configuration parser.
uart0Configure :: Mod CommandFields UART0Action
uart0Configure = command "configure" (info (pure UART0Configure) configureI)
    where configureI = mconcat [ fullDesc
                               , progDesc "Configure the UART0 bus."
                               ]

-- | UART0 enable parser.
uart0Enable :: Mod CommandFields UART0Action
uart0Enable = command "enable" (info (pure UART0Enable) enableI)
    where enableI = mconcat [ fullDesc
                            , progDesc "Enable the UART0 bus."
                            ]

-- | UART0 disable parser.
uart0Disable :: Mod CommandFields UART0Action
uart0Disable = command "disable" (info (pure UART0Disable) disableI)
    where disableI = mconcat [ fullDesc
                             , progDesc "Disable the UART0 bus."
                             ]

-- | UART0 read parser.
uart0Read :: Mod CommandFields UART0Action
uart0Read = command "read" (info (pure UART0Read) readI)
    where readI = mconcat [ fullDesc
                          , progDesc "Read a null-terminated string from the \
                                     \UART0 bus."
                          ]

-- | UART0 null-terminated string writing parser.
uart0WriteFromString :: Mod CommandFields UART0Action
uart0WriteFromString = command "write" ( info ( UART0WriteFromString
                                               <$> strArgument stringP
                                             )
                                             writeI
                                      )
    where stringP = mconcat [ metavar "PAYLOAD"
                            , help "The string to send over UART0."
                            ]
          writeI = mconcat [ fullDesc
                           , progDesc "Write a null-terminated string to the \
                                      \UART0 bus."
                           ]

-- | UART0 file writing parser.
uart0WriteFromFile :: Mod CommandFields UART0Action
uart0WriteFromFile = command "writefile" ( info ( UART0WriteFromString
                                                 <$> strArgument fileP
                                               )
                                               writeI
                                        )
    where fileP = mconcat [ metavar "FILEPATH"
                          , help "Path to local file."
                          ]
          writeI = mconcat [ fullDesc
                           , progDesc "Write a local file to the UART0 bus."
                           ]

-- | USART configuration parser.
usartConfigure :: Mod CommandFields USARTAction
usartConfigure = command "configure" (info (pure USARTConfigure) configureI)
    where configureI = mconcat [ fullDesc
                               , progDesc "Configure the USART bus."
                               ]

-- | USART enable parser.
usartEnable :: Mod CommandFields USARTAction
usartEnable = command "enable" (info (pure USARTEnable) enableI)
    where enableI = mconcat [ fullDesc
                            , progDesc "Enable the USART bus."
                            ]

-- | USART disable parser.
usartDisable :: Mod CommandFields USARTAction
usartDisable = command "disable" (info (pure USARTDisable) disableI)
    where disableI = mconcat [ fullDesc
                             , progDesc "Disable the USART bus."
                             ]

-- | USART read parser.
usartRead :: Mod CommandFields USARTAction
usartRead = command "read" (info (pure USARTRead) readI)
    where readI = mconcat [ fullDesc
                          , progDesc "Read a null-terminated string from the \
                                     \USART bus."
                          ]

-- | USART null-terminated string writing parser.
usartWriteFromString :: Mod CommandFields USARTAction
usartWriteFromString = command "write" ( info ( USARTWriteFromString
                                               <$> strArgument stringP
                                             )
                                             writeI
                                      )
    where stringP = mconcat [ metavar "PAYLOAD"
                            , help "The string to send over USART."
                            ]
          writeI = mconcat [ fullDesc
                           , progDesc "Write a null-terminated string to the \
                                      \USART bus."
                           ]

-- | USART file writing parser.
usartWriteFromFile :: Mod CommandFields USARTAction
usartWriteFromFile = command "writefile" ( info ( USARTWriteFromString
                                                 <$> strArgument fileP
                                               )
                                               writeI
                                        )
    where fileP = mconcat [ metavar "FILEPATH"
                          , help "Path to local file."
                          ]
          writeI = mconcat [ fullDesc
                           , progDesc "Write a local file to the USART bus."
                           ]

-- | USB configuration parser.
usbConfigure :: Mod CommandFields USBAction
usbConfigure = command "configure" (info (pure USBConfigure) configureI)
    where configureI = mconcat [ fullDesc
                               , progDesc "Configure the USB."
                               ]

-- | WDT configuration parser.
wdtConfigure :: Mod CommandFields WDTAction
wdtConfigure = command "configure" (info (pure WDTConfigure) configureI)
    where configureI = mconcat [ fullDesc
                               , progDesc "Configure the WDT."
                               ]
-- | Deprecate this.
moduleID :: Parser ModuleID
moduleID = argument (readParser parseModuleID) moduleP
    where moduleP = mconcat [ metavar "MODULE_ID"
                            , help "The module ID."
                            ]

-- | 'Word16' parser.
word16 :: Mod ArgumentFields Word16 -> Parser Word16
word16 = argument (readParser parseWord16)

-- | 'Word32' parser.
word32 :: Mod ArgumentFields Word32 -> Parser Word32
word32 = argument (readParser parseWord32)

-- | 'Bool' parser.
bool :: Mod ArgumentFields Bool -> Parser Bool
bool = argument (readParser parseBool)

-- | 'RGB' parser.
rgb :: Parser RGB
rgb = RGB <$> argument (readParser parseWord8) rP
          <*> argument (readParser parseWord8) gP
          <*> argument (readParser parseWord8) bP
    where rP = mconcat [ metavar "RED"
                       , help "8-bit red value."
                       ]
          gP = mconcat [ metavar "GREEN"
                       , help "8-bit green value."
                       ]
          bP = mconcat [ metavar "BLUE"
                       , help "8-bit red value."
                       ]

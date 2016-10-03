module Flipper.Console.Error where

import Data.Monoid

import Flipper.Error
import Flipper.Console.Action

consoleError :: Maybe ConsoleAction -> FlipperError -> String
consoleError c e = (commandCtx c) <> "\n" <> (errorMsg e)

commandCtx :: Maybe ConsoleAction -> String
commandCtx Nothing                = "No command context."
commandCtx (Just (Flash fp))      = "Flashing file " <> fp
commandCtx (Just (Install md fp)) = mconcat [ "Installing module "
                                            , show md
                                            , " from file "
                                            , fp
                                            ]
commandCtx (Just (Launch p))      = "Launching program " <> p
commandCtx (Just Reset)           = "Resetting device."
commandCtx (Just Suspend)         = "Suspending device."
commandCtx (Just Format)          = "Formatting device flash."
commandCtx (Just (ConsoleCall c)) = callCtx c

callCtx :: Call -> String
callCtx (ButtonCall b) = buttonCtx b
callCtx (FSCall f)     = fsCtx f
callCtx (GPIOCall g)   = gpioCtx g
callCtx (LEDCall l)    = ledCtx l
callCtx (SPICall s)    = spiCtx s
callCtx (UARTCall u)   = uartCtx u

buttonCtx :: ButtonAction -> String
buttonCtx ButtonRead = "Reading button state."

fsCtx :: FSAction -> String
fsCtx (FSCreateFromString fn p) = mconcat [ "Creating file "
                                          , fn
                                          , " from payload "
                                          , p
                                          ]
fsCtx (FSCreateFromFile fn fp)  = mconcat [ "Creating file "
                                          , fn
                                          , " from file "
                                          , fp
                                          ]
fsCtx (FSRemove fn)             = "Removing file " <> fn
fsCtx (FSRename t f)            = mconcat [ "Moving "
                                          , t
                                          , " to "
                                          , f
                                          ]

gpioCtx :: GPIOAction -> String
gpioCtx (GPIODigitalDirection p d) = mconcat [ "Setting direction of pin "
                                             , show p
                                             , " to "
                                             , show d
                                             ]
gpioCtx (GPIODigitalRead p)        = "Reading pin " <> show p
gpioCtx (GPIODigitalWrite p v)     = mconcat [ "Setting value of pin "
                                             , show p
                                             , " to "
                                             , show v
                                             ]
gpioCtx (GPIOAnalogDirection p d)  = mconcat [ "Setting direction of pin "
                                             , show p
                                             , " to "
                                             , show d
                                             ]
gpioCtx (GPIOAnalogRead p)         = "Reading pin " <> show p
gpioCtx (GPIOAnalogWrite p v)      = mconcat [ "Setting value of pin "
                                             , show p
                                             , " to "
                                             , show v
                                             ]

ledCtx :: LEDAction -> String
ledCtx (LEDSetRGB c) = "Setting LED color to " <> show c

spiCtx :: SPIAction -> String
spiCtx SPIEnable              = "Enabling SPI bus."
spiCtx SPIDisable             = "Disabling SPI bus."
spiCtx SPIRead                = "Reading from SPI bus."
spiCtx (SPIWriteFromString p) = "Writing to SPI bus: " <> p
spiCtx (SPIWriteFromFile fp)  = mconcat [ "Writing file"
                                        , fp
                                        , " to SPI bus."
                                        ]

uartCtx :: UARTAction -> String
uartCtx UARTEnable              = "Enabling UART bus."
uartCtx UARTDisable             = "Disabling UART bus."
uartCtx UARTRead                = "Reading from UART bus."
uartCtx (UARTWriteFromString p) = "Writing to UART bus: " <> p
uartCtx (UARTWriteFromFile fp)  = mconcat [ "Writing file"
                                        , fp
                                        , " to UART bus."
                                        ]

errorMsg :: FlipperError -> String
errorMsg OK                 = "OK, no error."
errorMsg MemAllocFailed     = "Memory allocation failed."
errorMsg Null               = "Null pointer encountered."
errorMsg Overflow           = "Overflow."
errorMsg NoDevice           = "No device present."
errorMsg NotAttached        = "Device is not attached."
errorMsg AlreadyAttached    = "Device is already attached."
errorMsg FileAlreadyExists  = "File already exists."
errorMsg FileNotFound       = "File not found."
errorMsg FMRPacketOverflow  = "FMR packet overflow."
errorMsg FMRError           = "FMR error."
errorMsg Endpoint           = "Endpoint error."
errorMsg USB                = "libusb error."
errorMsg Communication      = "Device communication error."
errorMsg Socket             = "Socket error."
errorMsg Module             = "Module not found."
errorMsg Resolution         = "Module dispatch resolution error."
errorMsg NoString           = "String not found."
errorMsg Checksum           = "Checksum error."
errorMsg Name               = "Name not found."
errorMsg Configuration      = "Cofiguration read error."
errorMsg Acknowledge        = "Device failed to acknowledge."
errorMsg Unknown            = "Unknown error."

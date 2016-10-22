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
callCtx (USARTCall u)  = usartCtx u

buttonCtx :: ButtonAction -> String
buttonCtx ButtonRead = "Reading button state."

fsCtx :: FSAction -> String
fsCtx (FSCreate fn)    = "Creating file " <> fn
fsCtx (FSDelete fn)    = "Deleting file " <> fn
fsCtx (FSSize fn)      = "Querying size of file " <> fn
fsCtx (FSOpen fn o)    = mconcat [ "Opening file "
                                 , fn
                                 , " at offset "
                                 , show o
                                 ]
fsCtx (FSPushString p) = "Pushing payload " <> p
fsCtx FSPullString     = "Pulling payload."
fsCtx FSClose          = "Closing open file."

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

usartCtx :: USARTAction -> String
usartCtx USARTEnable              = "Enabling USART bus."
usartCtx USARTDisable             = "Disabling USART bus."
usartCtx USARTRead                = "Reading from USART bus."
usartCtx (USARTWriteFromString p) = "Writing to USART bus: " <> p
usartCtx (USARTWriteFromFile fp)  = mconcat [ "Writing file"
                                            , fp
                                            , " to USART bus."
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

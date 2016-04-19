#!/usr/bin/env runhaskell

{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveAnyClass #-}

module Main where

import Control.Monad

import Data.Char

import Data.Maybe

import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Util

import System.Environment
import System.Exit
import System.FilePath

-- * Target Drivers

-- | Drivers for the libflipper target.
libflipper_drivers = [ "at45"
                     , "button" 
                     , "config"
                     , "error"
                     , "fdl"
                     , "fmr"
                     , "fs"
                     , "i2c"
                     , "io"
                     , "led"
                     , "pwm"
                     , "rtc"
                     , "sam"
                     , "spi"
                     , "temp"
                     , "timer"
                     , "usart"
                     , "usb"
                     , "wifi"
                     ]

-- | Drivers for the at91sam4s target.
at91sam4s_drivers = [ "at45"
                    , "button"
                    , "config"
                    , "error"
                    , "fdl"
                    , "fmr"
                    , "fs"
                    , "i2c"
                    , "io"
                    , "led"
                    , "pwm"
                    , "rtc"
                    , "spi"
                    , "temp"
                    , "timer"
                    , "usart"
                    , "usb"
                    , "wifi"
                    ]

-- | Drivers for the atmega16u2 target.
atmega16u2_drivers = [ "at45"
                     , "button"
                     , "config"
                     , "error"
                     , "fmr"
                     , "fs"
                     , "led"
                     , "sam"
                     , "spi"
                     , "usart"
                     , "usb"
                     , "wifi"
                     ]

-- * Preprocessor Options

-- | Preprocessor options for C avr targets.
avr_c_prep :: [String]
avr_c_prep = [ "-std=gnu99"
             , "-Os"
             , "-DARCH=ARCH_AVR8"
             , "-mmcu=atmega16u2"
             , "-D__AVR_ATMEGA16U2__"
             , "-D__atmega_build__"
             , "-DF_CPU=16000000UL"
             , "-D__osmium__"
             , "-Wno-pragmas"
             ]

-- | Preprocessor options for assembly avr targets.
avr_s_prep :: [String]
avr_s_prep = avr_c_prep ++ [ "-x"
                           , "assembler-with-cpp"
                           ]

-- | Preprocessor options for C arm targets.
arm_c_prep :: [String]
arm_c_prep = [ "-std=gnu99"
             , "-Os"
             , "-mcpu=arm7tdmi"
             , "-mtune=arm7tdmi"
             , "-mthumb"
             , "-mthumb-interwork"
             , "-nostartfiles"
             , "-ffreestanding"
             , "-fdata-sections"
             , "-ffunction-sections"
             , "-DF_CPU=48054850UL"
             , "-D__osmium__"
             , "-Wno-pragmas"
             ]

-- | Preprocessor options for assembly arm targets.
arm_s_prep :: [String]
arm_s_prep = arm_c_prep ++ [ "-x"
                           , "assembler-with-cpp"
                           ]

-- | Preprocessor options for native C targets.
native_prep :: [String]
native_prep = [ "-std=gnu99"
              , "-fpic"
              , "-D__verbose__"
              , "-Wno-#pragma-messages"
              ]

-- * Querying The Environment

-- | Include directories for all drivers.
driver_includes :: Action [FilePath]
driver_includes = (map (\d -> "drivers" </> d </> "include"))
              <$> getDirectoryDirs "drivers"

-- | The prefix for installation directories, read from the @PREFIX@
--   environment variable if present or defaulting to @/usr/local@ otherwise.
prefix :: Action FilePath
prefix = liftIO $ fromMaybe "/usr/local" <$> lookupEnv "PREFIX"

-- | Return 'Nothing' if the provided string is in the @PATH@, 'Just' the
--   relative path if it is.
which :: String -> Action (Maybe String)
which s = do
    (Exit c) <- command [EchoStdout False, EchoStderr False, Traced ""] "which" [s]
    case c of ExitSuccess -> return $ Just s
              _           -> return Nothing

-- | Determine under which name arm gcc is installed.
arm_gcc :: Action FilePath
arm_gcc = (fromMaybe (error "ARM gcc isn't available in $PATH") . msum)
        <$> sequence [ which "arm-elf-gcc"
                     , which "arm-elf-eabi-gcc"
                     , which "arm-eabi-newlib-gcc"
                     , which "arm-none-eabi-gcc"
                     ]

-- | Determine under which name arm objcopy is installed.
arm_objcopy :: Action FilePath
arm_objcopy = (fromMaybe (error "ARM objcopy isn't available in $PATH") . msum)
        <$> sequence [ which "arm-elf-objcopy"
                     , which "arm-elf-eabi-objcopy"
                     , which "arm-eabi-newlib-objcopy"
                     , which "arm-none-eabi-objcopy"
                     ]

-- | Determine under which name avr gcc is installed.
avr_gcc :: Action FilePath
avr_gcc = (fromMaybe (error "AVR gcc isn't available in $PATH") . msum)
        <$> sequence [which "avr-gcc"]

-- | Determine under which name avr objcopy is installed.
avr_objcopy :: Action FilePath
avr_objcopy = (fromMaybe (error "AVR objcopy isn't available in $PATH") . msum)
        <$> sequence [which "avr-objcopy"]

-- | Determine the native architecture.
arch :: Action String
arch = do
    (Stdout a) <- command [EchoStdout False, EchoStderr False, Traced ""] "uname" ["-m"]
    return ((filter (/= '\n') . map toLower) a)

-- | Determine that native target operating system.
target :: Action String
target = do
    (Stdout a) <- command [EchoStdout False, EchoStderr False, Traced ""] "uname" ["-s"]
    return ((filter (/= '\n') . map toLower) a)

-- | Determine the object format appropriate for the target and architecture.
objformat :: Action String
objformat = do
    t <- target
    case t of "darwin" -> return "mahcho64"
              "linux"  -> do a <- arch
                             case a of "x86_64" -> return "elf64"
                                       _        -> return "elf32"
              _        -> error "objformat: unknown target"

-- | Determine the shared object name appropriate for the target and
--   architecture.
dynlib :: Action String
dynlib = do
    t <- target
    case t of "darwin" -> return "libflipper.dylib"
              "linux"  -> return "libflipper.so"
              _        -> error ("dynlib: unknown target " ++ t)

-- | Determine the dynamically linked libraries appropriate for the target and
--   architecture.
libs :: Action [String]
libs = do
    t <- target
    case t of "darwin" -> return ["-framework", "CoreFoundation", "-framework", "IOKit"]
              "linux"  -> return ["-lusb"]
              _        -> error "libs: unknown target"

-- | Who am I?
whoami :: Action String
whoami = do
    (Stdout a) <- command [EchoStdout False, EchoStderr False, Traced ""] "sudo" ["whoami"]
    return (filter (/= '\n') a)

-- | Drop the fully qualified object file extension from a file name.
dropObjExts :: FilePath -> FilePath
dropObjExts = dropExtension . dropExtension

-- | Make a list of include directories into a list of compiler options.
mkIncFlags :: [FilePath] -> [String]
mkIncFlags = map ("-I" ++)

-- | Make a list of linker script paths into a list of compiler options.
mkLinkFlags :: [FilePath] -> [String]
mkLinkFlags = map ("-T" ++)

-- | Generic combinator for defining C compilation rules.
cRule :: Action FilePath -- ^ Action returning compiler path.
      -> [FilePath]      -- ^ Include directories.
      -> [FilePath]      -- ^ Preprocessor directives.
      -> FilePath        -- ^ Target
      -> Action ()
cRule comp inc prec o = do
    let c = dropObjExts o
        m = c -<.> "m"
        includes = mkIncFlags inc
    cc <- comp
    () <- command [] cc (prec ++ includes ++ [ "-c"
                                             , c
                                             , "-o"
                                             , o
                                             , "-MMD"
                                             , "-MF"
                                             , m
                                             ])
    needMakefileDependencies m

main :: IO ()
main = shakeArgs shakeOptions $ do

    want ["libflipper", "console", "osmium"]

    -- Top-level targets:
    phony "libflipper" $ do
        dyn <- dynlib
        need ["libflipper" </> dyn]

    phony "library" $ need ["libflipper"]

    phony "console" $ need ["console/flipper"]

    phony "osmium" $ need [ "osmium/targets/at91sam4s/osmium-sam4s.bin"
                          , "osmium/targets/atmega16u2/osmium-atmega.bin"
                          ]

    phony "native" $ need ["libflipper", "console"]

    phony "clean" $ removeFilesAfter "." [ "//*.o"
                                         , "//*.so"
                                         , "//*.dylib"
                                         , "//*.m"
                                         , "//*.elf"
                                         , "//*.bin"
                                         , "console/flipper"
                                         ]


    phony "install" $ need ["install-libflipper", "install-console"]

    phony "uninstall" $ do
        p   <- prefix
        dyn <- dynlib

        unit $ command [] "sudo" ["rm", "-f", p </> "lib" </> dyn]
        unit $ command [] "sudo" ["rm", "-f", p </> "include/flipper"]
        unit $ command [] "sudo" ["rm", "-f", p </> "include/flipper.h"]

    phony "install-libflipper" $ do
        p   <- prefix
        w   <- whoami
        dyn <- dynlib

        need ["libflipper"]

        -- Install shared library:
        unit $ command [] "sudo" ["cp", "libflipper" </> dyn, p </> "lib"]

        -- Install headers:
        let insth hp = do
            hs <- (map (hp </>)) <$> getDirectoryContents hp
            mapM (\h -> unit $ command [] "sudo" ["cp", "-R", h, p </> "include/"]) hs

        unit $ command [] "sudo" ["mkdir", "-p", p </> "include/flipper"]
        ((["libflipper/include", "include"] ++) <$> driver_includes) >>= mapM_ insth

    phony "install-console" $ do
        p <- prefix
        w <- whoami

        need ["console", "install-libflipper"]
        unit $ command [] "sudo" ["cp", "console/flipper", p </> "bin/"]

    phony "burn-at91sam4s" $ do
        need ["install-console", "osmium/targets/at91sam4s/osmium-sam4s.bin"]
        unit $ command [] "flipper" ["flash", "osmium/targets/at91sam4s/osmium-sam4s.bin"]

    phony "burn-atmega16u2" $ do
        need ["osmium/targets/atmega16u2/osmium-atmega.bin"]
        unit $ command [] "avrdude" [ "-p"
                                    , "atmega16u2"
                                    , "-B0.666"
                                    , "-c"
                                    , "usbtiny"
                                    , "-U"
                                    , "lfuse:w:0xFF:m"
                                    , "-U"
                                    , "hfuse:w:0xD9:m"
                                    , "-U"
                                    , "flash:w:osmium/targets/atmega16u2/osmium-atmega.bin"
                                    , "-U"
                                    , "eeprom:w:eeprom.hex"
                                    ]

    "libflipper/libflipper.so" %> \o -> do
        a  <- arch
        ss <- getDirectoryFiles "" $
                concat [ ["drivers" </> d </> "*/*.c" | d <- libflipper_drivers]
                       , [ "libflipper/architectures" </> a </> "*.asm"
                         , "libflipper/flipper/*.c"
                         , "libflipper/targets/linux//*.c"
                         ]
                       ]
        ls <- libs
        let os = map (<.> ".native.o") ss
        need os
        unit $ command [] "clang" $ ["-shared", "-rdynamic"] ++ os ++ ["-o", o] ++ ls

    "libflipper/libflipper.dylib" %> \o -> do
        ss <- getDirectoryFiles "" $
                concat [ ["drivers" </> d </> "*/*.c" | d <- libflipper_drivers]
                       , [ "libflipper/architectures/x86_64/*.asm"
                         , "libflipper/flipper/*.c"
                         , "libflipper/targets/darwin//*.c"
                         ] ]
        ls <- libs
        let os = map (<.> ".native.o") ss
        need os
        unit $ command [] "clang" $ ["-shared", "-rdynamic"] ++ os ++ ["-o", o] ++ ls

    "console/flipper" %> \o -> do
        ss  <- getDirectoryFiles "" ["console/*.c"]
        let os = map (<.> ".native.o") ss
        need $ "libflipper":os
        unit $ command [] "clang" $ os ++ ["-o", o, "-lflipper"]

    "osmium/targets/at91sam4s/osmium-sam4s.bin" %> \o -> do
        cc <- arm_gcc
        objcopy <- arm_objcopy
        ss <- getDirectoryFiles "" $
                concat [ ["drivers" </> d </> "common/*.c" | d <- at91sam4s_drivers]
                       , ["drivers" </> d </> "targets/at91sam4s//*.c" | d <- at91sam4s_drivers]
                       , ["drivers" </> d </> "targets/at91sam4s//*.s" | d <- at91sam4s_drivers]
                       , ["drivers" </> d </> "targets/shared//*.c" | d <- at91sam4s_drivers]
                       , ["osmium/targets/at91sam4s//*.c"]
                       , ["osmium/targets/at91sam4s//*.s"]
                       ]
        ls <- getDirectoryFiles "" ["osmium/targets/at91sam4s//*.ld"]
        let os  = map (<.> ".arm.o") ss
            o'  = o -<.> "elf"
            ls' = mkLinkFlags ls
        need (os ++ ls)
        unit $ command [] cc $ arm_c_prep ++ ls' ++ os ++ ["-o", o']
        unit $ command [] objcopy ["-O", "binary", o', o]

    "osmium/targets/atmega16u2/osmium-atmega.bin" %> \o -> do
        cc <- avr_gcc
        objcopy <- avr_objcopy
        ss <- getDirectoryFiles "" $
                concat [ ["drivers" </> d </> "common/*.c" | d <- atmega16u2_drivers]
                       , ["drivers" </> d </> "targets/atmega16u2//*.c" | d <- atmega16u2_drivers]
                       , ["drivers" </> d </> "targets/atmega16u2//*.s" | d <- atmega16u2_drivers]
                       , ["drivers" </> d </> "targets/shared//*.c" | d <- atmega16u2_drivers]
                       , ["osmium/targets/atmega16u2//*.c"]
                       , ["osmium/targets/atmega16u2//*.s"]
                       ]
        ls <- getDirectoryFiles "" ["osmium/targets/atmega16u2//*.ld"]
        let os  = map (<.> ".avr.o") ss
            o'  = o -<.> "elf"
            ls' = mkLinkFlags ls
        need (os ++ ls)
        unit $ command [] cc $ avr_c_prep ++ ls' ++ os ++ ["-o", o']
        unit $ command [] objcopy ["-O", "binary", o', o]

    -- Target-specific rules for object files:

    "//*.c.avr.o" %> \o -> do
        dis <- driver_includes
        let includes = dis ++ [ "include"
                              , "osmium/targets/atmega16u2/include"
                              ]
        cRule avr_gcc includes avr_c_prep o

    "//*.s.avr.o" %> \o -> do
        dis <- driver_includes
        let includes = dis ++ [ "include"
                              , "osmium/targets/atmega16u2/include"
                              ]
        cRule avr_gcc includes avr_s_prep o

    "//*.c.arm.o" %> \o -> do
        dis <- driver_includes
        let includes = dis ++ [ "include"
                              , "osmium/targets/at91sam4s/include"
                              ]
        cRule arm_gcc includes arm_c_prep o

    "//*.s.arm.o" %> \o -> do
        dis <- driver_includes
        let includes = dis ++ [ "include"
                              , "osmium/targets/at91sam4s/include"
                              ]
        cRule arm_gcc includes arm_s_prep o

    "//*.c.native.o" %> \o -> do
        dis <- driver_includes
        let includes = dis ++ [ "include"
                              , "libflipper/include"
                              , "console/include"
                              ]
        cRule (return "clang") includes native_prep o

    "//*.asm.native.o" %> \o -> do
        let s = dropObjExts o
            m = s -<.> "m"
        objfmt <- objformat
        unit $ command [] "nasm" [ "-f"
                                 , objfmt
                                 , "-o"
                                 , o
                                 , s
                                 , "-MD"
                                 , m
                                 ]
        needMakefileDependencies m

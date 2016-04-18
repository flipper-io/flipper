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

avr_c_prep :: [String]
avr_c_prep = [ "-std=gnu99"
             , "-Os"
             , "-DARCH=ARCH_AVR8"
             , "-mcpu=atmega16u2"
             , "-D__AVR_ATMEGA16U2__"
             , "-D__atmega_build__"
             , "-DF_CPU=16000000UL"
             , "-D__osmium__"
             , "-Wno-pragmas"
             ]

avr_s_prep :: [String]
avr_s_prep = avr_c_prep ++ [ "-x"
                           , "asembler-with-cpp"
                           ]

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
             , "D__osmium__"
             , "-Wno-pragmas"
             ]

arm_s_prep :: [String]
arm_s_prep = arm_c_prep ++ [ "-x"
                           , "asembler-with-cpp"
                           ]

native_prep :: [String]
native_prep = [ "-std=gnu99"
              , "-fpic"
              , "-D__verbose__"
              , "-Wno-#pragma-messages"
              ]

driver_includes :: Action [FilePath]
driver_includes = (map (\d -> "drivers" </> d </> "include"))
              <$> getDirectoryDirs "drivers"

getPrefix :: Action FilePath
getPrefix = liftIO $ fromMaybe "/usr/local" <$> lookupEnv "PREFIX"

inPath :: String -> Action (Maybe String)
inPath s = do
    (Exit c) <- command [EchoStdout False, EchoStderr False, Traced ""] "which" [s]
    case c of ExitSuccess -> return $ Just s
              _           -> return Nothing

getARMGCC :: Action FilePath
getARMGCC = (fromMaybe (error "ARM gcc isn't available in $PATH") . msum)
        <$> sequence [ inPath "arm-elf-gcc"
                     , inPath "arm-elf-eabi-gcc"
                     , inPath "arm-eabi-newlib-gcc"
                     ]

getARMObjCopy :: Action FilePath
getARMObjCopy = (fromMaybe (error "ARM objcopy isn't available in $PATH") . msum)
        <$> sequence [ inPath "arm-elf-objcopy"
                     , inPath "arm-elf-eabi-objcopy"
                     , inPath "arm-eabi-newlib-objcopy"
                     ]

getAVRGCC :: Action FilePath
getAVRGCC = (fromMaybe (error "AVR gcc isn't available in $PATH") . msum)
        <$> sequence [inPath "avr-gcc"]

getAVRObjCopy :: Action FilePath
getAVRObjCopy = (fromMaybe (error "AVR objcopy isn't available in $PATH") . msum)
        <$> sequence [inPath "avr-objcopy"]

arch :: Action String
arch = do
    (Stdout a) <- command [EchoStdout False, EchoStderr False, Traced ""] "uname" ["-m"]
    return ((filter (/= '\n') . map toLower) a)

target :: Action String
target = do
    (Stdout a) <- command [EchoStdout False, EchoStderr False, Traced ""] "uname" ["-s"]
    return ((filter (/= '\n') . map toLower) a)

objformat :: Action String
objformat = do
    t <- target
    case t of "darwin" -> return "mahcho64"
              "linux"  -> do a <- arch
                             case a of "x86_64" -> return "elf64"
                                       _        -> return "elf32"
              _        -> error "objformat: unknown target"

dynLibName :: Action String
dynLibName = do
    t <- target
    case t of "darwin" -> return "libflipper.dynlib"
              "linux"  -> return "libflipper.so"
              _        -> error ("dynLibName: unknown target " ++ t)

libs :: Action [String]
libs = do
    t <- target
    case t of "darwin" -> return ["-framework", "CoreFoundation", "-framework", "IOKit"]
              "linux"  -> return ["-lusb"]
              _        -> error "libs: unknown target"

dropObjExts :: FilePath -> FilePath
dropObjExts = dropExtension . dropExtension

mkIncFlags :: [FilePath] -> [String]
mkIncFlags = map ("-I" ++)

mkLinkFlags :: [FilePath] -> [String]
mkLinkFlags = map ("-T" ++)

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
    -- Top-level targets:
    phony "libflipper" $ do
        dyn <- dynLibName
        need ["libflipper" </> dyn]

    phony "library" $ need ["libflipper"]

    phony "console" $ need ["console/flipper"]

    phony "osmium" $ need ["osmium/targets/at91sam4s/osmium-sam4s.bin", "osmium/targets/atmega16u2/osmium-atmega.bin"]

    phony "native" $ need ["library", "console"]

    phony "clean" $ removeFilesAfter "." ["//*.o", "//*.so", "//*.dylib", "//*.m", "//*.bin"]

    want ["libflipper", "consolve", "osmium"]

    "libflipper/libflipper.so" %> \o -> do
        a  <- arch
        ss <- getDirectoryFiles "" [ "drivers/*/*/*.c"
                                   , "libflipper/architectures" </> a </> "*.asm"
                                   , "libflipper/flipper/*.c"
                                   , "libflipper/targets/linux//*.c"
                                   ]
        ls <- libs
        let os = map (<.> ".native.o") ss
        need os
        unit $ command [] "clang" $ ["-shared", "-rdynamic"] ++ os ++ ["-o", o] ++ ls

    "libflipper.dylib" %> \o -> do
        ss <- getDirectoryFiles "" [ "drivers/*/*/*.c"
                                   , "libflipper/architectures/x86_64/*.asm"
                                   , "libflipper/flipper/*.c"
                                   , "libflipper/targets/darwin//*.c"
                                   ]
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
        cc <- getARMGCC
        objcopy <- getARMObjCopy
        ss <- getDirectoryFiles "" [ "drivers/*/common/*.c"
                                   , "drivers/*/targets/at91sam4s//*.c"
                                   , "osmium/targets/at91sam4s//*.c"
                                   ]
        ls <- getDirectoryFiles "" ["osmium/targets/at91sam4s//*.ld"]
        let os  = map (<.> ".arm.o") ss
            o'  = o -<.> "elf"
            ls' = mkLinkFlags ls
        need (os ++ ls)
        unit $ command [] cc $ ls' ++ os ++ ["-o", o']
        unit $ command [] objcopy ["-O", "binary", o', o]

    "osmium/targets/atmega16u2/osmium-atmega.bin" %> \o -> do
        cc <- getAVRGCC
        objcopy <- getAVRObjCopy
        ss <- getDirectoryFiles "" [ "drivers/*/common/*.c"
                                   , "drivers/*/targets/atmega16u2//*.c"
                                   , "osmium/targets/atmega16u2//*.c"
                                   ]
        ls <- getDirectoryFiles "" ["osmium/targets/atmega16u2//*.ld"]
        let os  = map (<.> ".avr.o") ss
            o'  = o -<.> "elf"
            ls' = mkLinkFlags ls
        need (os ++ ls)
        unit $ command [] cc $ ls' ++ os ++ ["-o", o']
        unit $ command [] objcopy ["-O", "binary", o', o]

    -- Target-specific rules for object files:

    "//*.c.avr.o" %> \o -> do
        dis <- driver_includes
        let includes = dis ++ [ "include"
                              , "osmium/targets/atmega16u2/include"
                              ]
        cRule getAVRGCC includes avr_c_prep o

    "//*.s.avr.o" %> \o -> do
        dis <- driver_includes
        let includes = dis ++ [ "include"
                              , "osmium/targets/atmega16u2/include"
                              ]
        cRule getAVRGCC includes avr_s_prep o

    "//*.c.arm.o" %> \o -> do
        dis <- driver_includes
        let includes = dis ++ [ "include"
                              , "osmium/targets/at91sam4s/include"
                              ]
        cRule getARMGCC includes arm_c_prep o

    "//*.s.arm.o" %> \o -> do
        dis <- driver_includes
        let includes = dis ++ [ "include"
                              , "osmium/targets/at91sam4s/include"
                              ]
        cRule getARMGCC includes arm_s_prep o

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
        () <- command [] "nasm" [ "-f"
                                , objfmt
                                , "-o"
                                , o
                                , s
                                , "-MD"
                                , m
                                ]
        needMakefileDependencies m

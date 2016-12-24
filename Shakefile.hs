#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveDataTypeable
           , DeriveGeneric
           , GeneralizedNewtypeDeriving
           #-}

module Main where

import Control.Monad

import Data.Char

import Data.Maybe

import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import Development.Shake.Util

import GHC.Generics

import System.Exit

-- * Preprocessor Options

-- | Preprocessor options for AVR targets.
avr_c_prep :: [String]
avr_c_prep = [ -- Use C99:
               "-std=c99"
             , -- Optimize for size:
               "-Os"
             , -- Arch is AVR8:
               "-DARCH=ARCH_AVR8"
             , -- CPU is ATMEGA16U2:
               "-D__AVR_ATMEGA16U2__"
             , -- Clock freq is 16 MHz:
               "-DF_CPU=16000000UL"
             , -- Don't warn about pragmas:
               "-Wno-pragmas"
             , -- MCU is ATMEGA16U2:
               "-mmcu=atmega16u2"
             , -- Platform header:
             "-DPLATFORM_HEADER=<platform/atmega16u2.h>"
             ]

-- | Preprocessor options for ARM targets.
arm_c_prep :: [String]
arm_c_prep = [ -- Use C99.
               "-std=c99"
             , -- Optimize for size:
               "-Os"
             , -- CPU is ARM Cortex M4:
               "-mcpu=cortex-m4"
             , -- Arch is ARMv7E-M:
               "-march=armv7e-m"
             , -- Use Thumb ISA:
               "-mthumb"
             , -- Include debugging metadata:
               "-g"
             , -- Platform header:
               "-DPLATFORM_HEADER=<platform/atsam4s16b.h>"
             ]

-- | Preprocessor options for native C targets.
native_prep :: [String]
native_prep = [ -- Use C99:
                "-std=c99"
              , -- Generate relocatable code:
                "-fpic"
              , -- Enable flipper constructors:
                "-D__flipper_constructors__"
              , -- libflipper errors abort the calling program by default:
                "-D__enable_error_side_effects__"
              , -- Enables FS transfers:
                "-D__fs_transfer_symbols__"
              , -- Enables FDL stuff:
                "-D__fld_upload_symbols__"
              , -- Include debugging metadata:
                "-g"
              , -- Platform header:
                "-DPLATFORM_HEADER=<platform/posix.h>"
              ]

-- * Querying The Environment

-- | Make a path absolute.
realpath :: FilePath -> Action FilePath
realpath p = do
    Stdout p' <- command [EchoStdout False, EchoStderr False, Traced ""]
                         "realpath"
                         [p]
    return p'

-- | The prefix for installation directories, read from the @PREFIX@
--   environment variable if present or defaulting to @/usr/local@ otherwise.
prefix :: Action FilePath
prefix = fromMaybe "/usr/local" <$> getEnv "PREFIX"

-- | Return 'Nothing' if the provided string isn't in the @PATH@, 'Just' the
--   relative path if it is.
which :: String -> Action (Maybe String)
which s = do
    (Exit c) <- command [EchoStdout False, EchoStderr False, Traced ""]
                "which"
                [s]
    case c of ExitSuccess -> return $ Just s
              _           -> return Nothing

-- | Determine under which name arm gcc is installed.
arm_gcc :: Action FilePath
arm_gcc = (fromMaybe (error e) . msum) <$> sequence [which "arm-none-eabi-gcc"]
    where e = "arm-none-eabi-gcc isn't available in $PATH"

-- | Determine under which name arm objcopy is installed.
arm_objcopy :: Action FilePath
arm_objcopy = (fromMaybe (error e) . msum)
          <$> sequence [which "arm-none-eabi-objcopy"]
    where e = "ARM objcopy isn't available in $PATH"

-- | Determine under which name avr gcc is installed.
avr_gcc :: Action FilePath
avr_gcc = (fromMaybe (error e) . msum) <$> sequence [which "avr-gcc"]
    where e = "AVR gcc isn't available in $PATH"

-- | Determine under which name avr objcopy is installed.
avr_objcopy :: Action FilePath
avr_objcopy = (fromMaybe (error e) . msum) <$> sequence [which "avr-objcopy"]
    where e = "AVR objcopy isn't available in $PATH"

-- | Determine which compiler to use for native code.
--   - If $CC is set, that compiler is used.
--   - Else, if clang is available on the @$PATH@, that is used.
--   - Else, if gcc is available on the @PATH@, that is used.
cc :: Action FilePath
cc = do
    cv <- getEnv "CC"
    case cv of Just p  -> pure p
               Nothing -> (fromMaybe (error e) . msum)
                          <$> sequence [ which "clang"
                                       , which "gcc"
                                       ]
    where e = "clang nor gcc are available in $PATH"

-- | Determine the native architecture.
arch :: Action String
arch = do
    (Stdout a) <- command [EchoStdout False, EchoStderr False, Traced ""]
                          "uname"
                          ["-m"]
    return ((filter (/= '\n') . map toLower) a)

-- | Determine that native target operating system.
target :: Action String
target = do
    (Stdout a) <- command [EchoStdout False, EchoStderr False, Traced ""]
                          "uname"
                          ["-s"]
    return ((filter (/= '\n') . map toLower) a)

-- | Determine the object format appropriate for the target and architecture.
objformat :: Action String
objformat = do
    t <- target
    case t of "darwin" -> return "macho64"
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
--   architecture. For @pkg-config@ packages, use 'pkgs' instead.
libs :: Action [String]
libs = do
    t <- target
    case t of "darwin" -> return []
              "linux"  -> return ["-ldl"]
              _        -> error "libs: unknown target"

-- | Determine the dynamically linked libraries appropriate for the target and
--   architecture.
pkgs :: Action [String]
pkgs = do
    t <- target
    case t of "darwin" -> return ["libusb-1.0"]
              "linux"  -> return ["libusb-1.0"]
              _        -> error "libs: unknown target"

-- * Building Stuff

-- | Drop the fully qualified object file extension from a file name.
dropObjExts :: FilePath -> FilePath
dropObjExts = dropExtension . dropExtension

-- | Make a list of include directories into a list of compiler options.
mkIncFlags :: [FilePath] -> [String]
mkIncFlags = map ("-I" ++)

-- | Make a list of linker script paths into a list of compiler options.
mkLinkFlags :: [FilePath] -> [String]
mkLinkFlags = map ("-Wl,-T" ++)

-- | Run a list of 'Action's to generate a list of dependencies.
findDeps :: [Action [FilePath]] -> Action [FilePath]
findDeps = fmap join . sequence

-- | All module directories.
modules :: Action [FilePath]
modules = map ("modules" </>) <$> getDirectoryDirs "modules"

-- | All module shared sources.
modSharedSrc :: Action [FilePath]
modSharedSrc = modules >>= (getDirectoryFiles "" . map (</> "src/*.c"))

-- | All module AVR sources.
modAVRSrc :: Action [FilePath]
modAVRSrc = modules >>= ( getDirectoryFiles ""
                        . map (</> "targets/atmega16u2/*.c")
                        )

-- | All module ARM sources.
modARMSrc :: Action [FilePath]
modARMSrc = modules >>= ( getDirectoryFiles ""
                        . map (</> "targets/atsam4s16b/*.c")
                        )

-- | All module FMR sources.
modFMRSrc :: Action [FilePath]
modFMRSrc = modules >>= ( getDirectoryFiles ""
                        . map (</> "targets/fmr/*.c")
                        )

-- | Include directories for all modules.
modIncludes :: Action [FilePath]
modIncludes = map (</> "include") <$> modules

-- | Prefix a path with @build@, used for translating a source file path to a
--   build artifact path.
buildpref :: FilePath -> FilePath
buildpref = ("build" </>)

-- | A @pkg-config@ query.
newtype PkgConfigQuery = PkgConfigQuery String
                       deriving ( Eq
                                , Show
                                , Typeable
                                , Hashable
                                , Binary
                                , NFData
                                )

-- | The result of a @pkg-config@ query.
data PkgConfigFlags = PkgConfigFlags {
    -- | Package C flags.
    cflags :: [String]
    -- | Package linker flags.
  , ldflags :: [String]
  } deriving ( Eq
             , Show
             , Generic
             , Typeable
             )

instance Hashable PkgConfigFlags
instance Binary PkgConfigFlags
instance NFData PkgConfigFlags

-- | Query @pkg-config@ for a package's C flags and linker flags.
pkgconfig :: PkgConfigQuery -> Action PkgConfigFlags
pkgconfig (PkgConfigQuery pkg) = do
    (Stdout cf) <- command [EchoStdout False]
                           "pkg-config"
                           ["--cflags", pkg]
    (Stdout lf) <- command [EchoStdout False]
                           "pkg-config"
                           ["--libs", pkg]
    pure (PkgConfigFlags (words cf) (words lf))

-- | Generic combinator for defining C compilation rules.
cRule :: Action FilePath  -- ^ Action returning compiler path.
      -> [FilePath]       -- ^ Include directories.
      -> [FilePath]       -- ^ Preprocessor directives.
      -> FilePath         -- ^ Target.
      -> Action ()
cRule comp inc prec o = do
    let c = dropDirectory1 (dropObjExts o)
        m = buildpref (c -<.> "mk")
        includes = mkIncFlags inc
    cc <- comp
    command_ [] cc (prec ++ includes ++ [ "-c"
                                        , c
                                        , "-o"
                                        , o
                                        , "-MMD"
                                        , "-MF"
                                        , m
                                        ])
    needMakefileDependencies m

-- | Generic combinator for defining archiver rules.
arRule :: Action FilePath -- ^ Action returning archiver path.
       -> [FilePath]      -- ^ Object files.
       -> FilePath        -- ^ Target.
       -> Action ()
arRule ara os a = do
    need os
    ar <- ara
    command_ [] ar ("rvs" : a : os)

-- | Generic combinator for defining linker rules.
ldRule :: Action FilePath -- ^ Action returning linker path.
       -> [String]        -- ^ Linker options.
       -> [FilePath]      -- ^ Object files.
       -> FilePath        -- ^ Target.
       -> Action ()
ldRule lda ops os so = do
    need os
    ld <- lda
    command_ [] ld (os ++ ["-o", so] ++ ops)

-- | Combinator for installation commands. Use @sudo@ if we're on Linux and
--   aren't root, otherwise, don't.
instCmd :: CmdResult r => [CmdOption] -> [String] -> Action r
instCmd os (c:cs) = do
    t <- target
    case t of "darwin" -> command os c cs
              _        -> do (Stdout a) <- command [ EchoStdout False
                                                   , EchoStderr False
                                                   , Traced ""
                                                   ]
                                                   "whoami"
                                                   []
                             case filter (/= '\n') a of
                                 "root" -> command os c cs
                                 _      -> command os "sudo" (c:cs)

-- | Like 'instCmd' but discards the result.
instCmd_ :: [CmdOption] -> [String] -> Action ()
instCmd_ os cs = unit $ instCmd os cs

main :: IO ()
main = shakeArgs (shakeOptions { shakeThreads = 0 }) $ do

    -- Set up the @pkg-config@ oracle:
    addOracle pkgconfig

    -- By default we build libflipper, the console, and osmium for all targets:
    want ["flipper-library", "flipper-console", "flipper-osmium"]

    -- Builds libflipper:
    phony "flipper-library" $ do

        -- Find out how to name dynamic libraries on this platform:
        dyn <- dynlib

        -- Build the library:
        need ["build/libflipper" </> dyn]

    -- Builds the console:
    phony "flipper-console" $ do

        -- We need libflipper to build the console:
        need ["flipper-library"]

        -- We need an absolute path to libflipper to pass to stack:
        lp <- realpath "build/libflipper"

        -- Build the console with stack:
        command_ [] "stack" [ "--stack-yaml=languages/haskell/console/stack.yaml"
                            , "--extra-lib-dirs=" ++ lp
                            , "install"
                            , "--local-bin-path"
                            , "../../../build/console/"
                            ]

    -- Builds osmium for all targets:
    phony "flipper-osmium" $ need [ "build/osmium/osmium-atmega16u2.hex"
                                  , "build/osmium/osmium-atsam4s16b.bin"
                                  ]

    -- Build all native code, i.e. libflipper and the console:
    phony "native" $ need ["flipper-library", "flipper-console"]

    -- Remove all build artifacts:
    phony "clean" $ do

        -- Stack cleans up after itself:
        command_ [] "stack" [ "--stack-yaml=languages/haskell/console/stack.yaml"
                            , "clean"
                            ]

        -- Delete everything in the build directory:
        removeFilesAfter "build" ["//*"]

    -- Install libflipper and the console:
    phony "install" $ do
        need ["flipper-library", "flipper-console"]
        need ["install-libflipper"]
        need ["install-console"]

    -- Uninstall libflipper and the console:
    phony "uninstall" $ do
        p   <- prefix
        dyn <- dynlib

        instCmd_ [] ["rm", "-f", p </> "lib" </> dyn]
        instCmd_ [] ["rm", "-rf", p </> "include/flipper"]
        instCmd_ [] ["rm", "-f", p </> "include/flipper.h"]
        instCmd_ [] ["rm", "-f", p </> "bin/flipper"]

    -- Install libflipper and the flipper header files:
    phony "install-libflipper" $ do
        -- libflipper needs to be built before we can install it:
        need ["flipper-library"]

        p   <- prefix
        dyn <- dynlib

        -- Make the @$PREFIX/lib@ directory:
        instCmd_ [] ["mkdir", "-p", p </> "lib"]

        -- Install the shared library:
        instCmd_ [] ["cp",  "build/libflipper" </> dyn, p </> "lib/"]

        -- Make the @$PREFIX/include/flipper@ directory:
        instCmd_ [] ["mkdir", "-p", p </> "include/flipper"]

        -- Install the top-level headers:
        instCmd_ [] ["cp", "include/flipper.h", p </> "include/"]
        instCmd_ [] ["cp", "-R", "include/flipper", p </> "include/"]

        -- Install module headers:
        modIncludes >>= mapM_ (\h -> instCmd_ [] [ "cp"
                                                 , "-R"
                                                 , h </> "flipper"
                                                 , p </> "include/"
                                                 ])

        -- Install POSIX platform headers:
        instCmd_ [] ["cp"
                    , "-R"
                    , "platforms/posix/include/platform"
                    , p </> "include/flipper/"
                    ]

    -- Install the console:
    phony "install-console" $ do

        -- Find out what the installation prefix is:
        p <- prefix

        -- The console needs to be built before we can install it:
        need ["flipper-console"]

        -- Make the @$PREFIX/bin@ directory:
        instCmd_ [] ["mkdir", "-p", p </> "bin"]

        -- Copy the console to the installation target:
        instCmd_ [] ["cp", "build/console/flipper", p </> "bin/"]

    -- Install osmium on the ATSAM4S:
    phony "flash-atsam4s16b" $ do

        -- We need the console and the osmium image to upload to the device:
        need ["flipper-console", "build/osmium/osmium-atsam4s16b.bin"]

        -- Use the console to flash the image to the device:
        command_ [] "build/console/flipper" ["flash", "build/osmium/osmium-atsam4s16b.bin"]

    -- Install osmium on the ATMEGA16U2:
    phony "flash-atmega16u2" $ do

        -- We need the osmium image to upload to the device:
        need ["build/osmium/osmium-atmega16u2.hex"]

        -- Erase the device:
        command_ [] "dfu_programmer" [ "at90usb162"
                                     , "erase"
                                     , "--force"
                                     ]

        -- Flash the image to the device:
        command_ [] "dfu_programmer" [ "at90usb162"
                                     , "flash"
                                     , "build/osmium/osmium-atmega16u2.hex"
                                     ]

        -- Launch osmium on the ATMEGA16U2:
        command_ [] "dfu_programmer" [ "at90usb162"
                                     , "launch"
                                     , "--no-reset"
                                     ]

    -- Build libflipper:
    "build/libflipper/libflipper.*" %> \o -> do

        -- Find the sources needed to build libflipper:
        ss <- findDeps [ -- Finds modules/*/src/*.c
                         modSharedSrc
                         -- Finds modules/*/targets/fmr/*.c
                       , modFMRSrc
                       , getDirectoryFiles ""  [ "libflipper/src//*.c"
                                               , "platforms/posix/src//*.c"
                                               ]
                       ]

        -- Find out what packages we need to link against on this platform:
        ps <- pkgs

        -- Find out what libraries we need to link against on this platform:
        ls <- libs

        -- Get the linker flags with @pkg-config@:
        ldfs <- (>>= ldflags) <$> mapM (askOracle . PkgConfigQuery) ps

        -- Build the list of necessary object files from the list of necessary
        -- source files:
        let os = map (buildpref . (<.> ".native.o")) ss

        -- Run the linker:
        ldRule cc ("-shared" : (ls ++ ldfs)) os o

    -- Build the osmium hex image for the ATMEGA16U2:
    "build/osmium/osmium-atmega16u2.hex" %> \o -> do

        -- We need the ATMEGA16U2 osmium ELF image to build the hex image:
        need ["build/osmium/osmium-atmega16u2.elf"]

        -- Determine under which name arm objcopy is installed:
        objcopy <- avr_objcopy

        -- Run objcopy:
        command_ [] objcopy [ "-O"
                            , "ihex"
                            , "build/osmium/osmium-atmega16u2.elf"
                            , o
                            ]

    -- Build the osmium binary image for the ATSAM4S16B:
    "build/osmium/osmium-atsam4s16b.bin" %> \o -> do

        -- We need the ATSAM4S16B osmium ELF image to build the binary image:
        need ["build/osmium/osmium-atsam4s16b.elf"]

        -- Determine under which name arm objcopy is installed:
        objcopy <- arm_objcopy

        -- Run objcopy:
        command_ [] objcopy [ "-O"
                            , "binary"
                            , "build/osmium/osmium-atsam4s16b.elf"
                            , o
                            ]

    -- Build the osmium ELF for the ATMEGA16U2:
    "build/osmium/osmium-atmega16u2.elf" %> \o -> do

        -- Find the sources needed to build osmium for the ATMEGA16U2:
        ss <- findDeps [ -- Finds modules/*/src/*.c
                         modSharedSrc
                         -- Finds modules/*/targets/atmega16u2/*.c
                       , modAVRSrc
                       , getDirectoryFiles "" [ "osmium/src//*.c"
                                              , "libflipper/src/crc.c"
                                              , "libflipper/src/fmr.c"
                                              , "platforms/atmega16u2//*.c"
                                              , "platforms/atmega16u2//*.S"
                                              ]
                       ]

        -- Build the list of necessary object files from the list of necessary
        -- source files:
        let os = map (buildpref . (<.> ".avr.o")) ss

            -- Linker flags:
            ls = [ "-Wl,--start-group"
                 , "-Wl,-Bdynamic"
                 , "-mmcu=atmega16u2"
                 ]

        -- Run the linker:
        ldRule avr_gcc ls os o

    -- Build the osmium ELF for the ATSAM4S16B:
    "build/osmium/osmium-atsam4s16b.elf" %> \o -> do

        -- Find the sources needed to build osmium for the ATSAM4S16B:
        ss <- findDeps [ -- Finds modules/*/src/*.c
                         modSharedSrc
                         -- Finds modules/*/targets/atsam4s16b/*.c
                       , modARMSrc
                       , getDirectoryFiles "" [ "osmium//*.c"
                                              , "libflipper/src/crc.c"
                                              , "libflipper/src/fmr.c"
                                              , "platforms/atsam4s16b//*.c"
                                              , "platforms/atsam4s16b//*.S"
                                              ]
                       ]

        -- Find the linker scripts needed to build osmium for the ATSAM4S16B:
        lds <- mkLinkFlags <$> getDirectoryFiles "" ["platforms/atsam4s16b//*.ld"]

        -- Build the list of necessary object files from the list of necessary
        -- source files:
        let os = map (buildpref . (<.> ".arm.o")) ss

            -- Linker flags:
            ls = lds ++ [ "-Wl,--start-group"
                        , "-Wl,-Bdynamic"
                        , "-nostartfiles"
                        ]

        -- Run the linker:
        ldRule arm_gcc ls os o

    -- Generic rule for compiling C or assembling for the ATMEGA16U2:
    ["build//*.c.avr.o", "build//*.S.avr.o"] |%> \o -> do

        -- Find the include files necessary for compiling C or assembling for
        -- the ATMEGA16U2:
        is <- findDeps [ -- Finds modules/*/include/
                         modIncludes
                       , pure [ "include/"
                              , "osmium/include"
                              , "platforms/atmega16u2/include"
                              ]
                       ]

        -- Run the C compiler:
        cRule avr_gcc is avr_c_prep o

    -- Generic rule for compiling C or assembling for the ATSAM4S16B:
    ["build//*.c.arm.o", "build//*.S.arm.o"] |%> \o -> do

        -- Find the include files necessary for compiling C or assembling for
        -- the ATSAM4S16B:
        is <- findDeps [ --Finds modules/*/include/
                         modIncludes
                       , pure [ "include"
                              , "osmium/include"
                              , "platforms/atsam4s16b/include"
                              ]
                       ]

        -- Run the C compiler:
        cRule arm_gcc is arm_c_prep o

    -- Generic rule for compiling C for the native platform:
    "build//*.c.native.o" %> \o -> do

        -- Find the include files necessary for compiling C for the native
        -- platform:
        is <- findDeps [ modIncludes
                       , pure [ "include"
                              , "platforms/posix/include"
                              ]
                       ]

        -- Get the C flags for @libusb-1.0@ with @pkg-config@:
        (PkgConfigFlags cf _) <- askOracle $ PkgConfigQuery "libusb-1.0"

        -- Run the C compiler:
        cRule cc is (cf ++ native_prep) o

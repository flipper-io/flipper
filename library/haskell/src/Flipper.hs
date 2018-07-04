{-|
Module      : Flipper
Description : Flipper Device Interface
Copyright   : George Morgan, Travis Whitaker 2016
License     : All rights reserved.
Maintainer  : travis@flipper.io
Stability   : Provisional
Portability : Windows, POSIX

Flipper is an embedded development platform with first-class support for many
programming languages. See <http://flipper.io> for more information.

= Attaching to Flipper

In order to control the Flipper device, it must be attached over one of several
provided interfaces, including USB, TCP/UDP, and Bluetooth. If multiple
Flipper devices are attached to the same process, then a single distinguished
active device is designated to receive commands. Only one device may be
designated as active at a time and this designation is global across an instance
of a Flipper-controlling program. However, any other device may be designated by
name at any time. When a new device is attached, it is automatically designated
as the active device. If the active device is detached, then the device attached
immediately before the detaching device is the new active device. Only one
program may attach to a Flipper device at once.

This module, like all others in this package, is intended to be imported
@qualified@, e.g.

> import qualified Flipper

= Interacting with Flipper

Most functions in this package return into a monad in the 'MonadFlipper' class.
A 'MonadFlipper' instance is provided for 'IO', making it easy to interact with
Flipper in small programs or GHCi. For example, to turn Flipper's LED blue:

>>> import qualified Flipper
>>> import qualified Flipper.LED as LED
>>> Flipper.attach (USB Nothing)
True
>>> LED.setRGB (RGB 0 0 255)

Or to blink the LED at 2 Hz:

> module Main where
>
> import qualified Flipper
> import qualified Flipper.LED as LED
>
> import Control.Concurrent (threadDelay)
>
> blink :: IO ()
> blink = do LED.setRGB blue
>            threadDelay 500000
>            LED.setRGB off
>            threadDelay 500000
>    where blue = RGB 0 0 255
>          off  = RGB 0 0 0
>
> main :: IO ()
> main = Flipper.attach (USB Nothing) >> forever blink

When interacting with Flipper in the 'IO' monad, any faults detected via
Flipper's error reporting mechanism will throw Haskell exceptions. There are
more sophisticated monads provided in the "Flipper.MonadFlipper" module that
provide type-safe and deterministic handling for Flipper errors.

= Exchanging Data with Flipper

If a Haskell data type is analogous to a C struct in use on the device, then an
instance of the 'Bufferable' class may be derived. Functions in this package for
controlling Flipper's buses can send or receive any 'Bufferable' data. For
example:

> -- Needs -XDeriveGeneric, -XDeriveAnyClass
>
> module SomeStruct where
>
> import qualified Flipper.Bufferable as Buf
> import qualified Flipper.USB        as USB
>
> import qualified Data.ByteString as B
>
> import GHC.Generics
>
> data SomeStruct = SomeStruct {
>     ssID      :: Int
>   , ssName    :: String
>   , ssPayload :: B.ByteString
>   } deriving (Generic, Bufferable)
>
> sendSomeStructUSB = USB.push
> recvSomeStructUSB = USB.pull

There are data types provided in the "Flipper.Bufferable" module for specifying
the relationship between a Haskell data type and the analogous C struct.
Alternatively custom serializers may be specified with the 'Put' builder monoid
provided by the "Flipper.Put" module, and custom parsers may be specified with
the 'Get' monad provided by the "Flipper.Get" module.

= Flipper and Haskell Threads

This package wraps functions from the @libflipper@ C library, which is not
thread-safe and relies on thread-local global state. For a program that uses
multiple operating system threads (e.g. a Haskell program linked with the
@-threaded@ option), this means that multiple functions that return into
'MonadFlipper' must /not/ be called concurrently, and all such operations must
be performed by the same operating system thread. In GHC this may be
accomplished with bound threads, see the corresponding section in the module
documentation for "Control.Concurrent". If your program does not make use of
Haskell threads (i.e. no calls to 'forkIO') or is not linked with the
@-threaded@ option then there is nothing to worry about.
-}

module Flipper (
    -- * Flipper Device Endpoints
    I.Endpoint(..)
    -- * Device Attachment, Selection, and Detachment
  , select
  , attach
  , detach
  ) where

import Flipper.MonadFlipper

import qualified Flipper.Internal.Flipper as I

-- | Select a new active device by device name. Returns 'True' if a device with
--   the provided name is attached to the computer, 'False' otherwise.
select :: MonadFlipper m => String -> m Bool
select = bracketIO . I.select

-- | Attach a new device via the provided 'I.Endpoint'. Returns 'True' if a
--   device was available at the provided endpoint, 'False' otherwise.
attach :: MonadFlipper m => I.Endpoint -> m Bool
attach = bracketIO . I.attach

-- | Detach a device by device name. Returns 'True' if a device with the
--   provided name was attached to the computer, 'False' otherwise.
detach :: MonadFlipper m => String -> m Bool
detach = bracketIO . I.detach

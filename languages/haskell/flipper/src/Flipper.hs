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

In order to control the Flipper device, it must be attached over one of several
provided interfaces, including USB, TCP/UDP, and Bluetooth. If multiple
Flipper devices are attached to the same computer, then a single distinguished
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

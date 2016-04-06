{-# LANGUAGE FlexibleInstances #-}

module Flipper.Bufferable where

import Flipper.Get
import Flipper.Put

import Foreign.Storable

class Bufferable a where
    put :: a -> Put
    get :: Get a

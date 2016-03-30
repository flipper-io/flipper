module Flipper.Internal.Flipper where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils


-- | An endpoint via which a Flipper device may be attached.
data Endpoint = USB     -- ^ Local connection via USB.
              | Network -- ^ Remote connection via TCP/UDP.
              | FVM     -- ^ Flipper simulation via FVM.

enumEndpoint :: Endpoint -> CInt
enumEndpoint USB     = 0
enumEndpoint Network = 1
enumEndpoint FVM     = 2

select :: String -> IO Bool
select n = withCString n (((not . toBool) <$>) . c_flipper_select)

attach :: Endpoint -> String -> IO Bool
attach e n = withCString n (((not . toBool) <$>) . c_flipper_attach (enumEndpoint e))

detach :: String -> IO Bool
detach n = withCString n (((not . toBool) <$>) . c_flipper_detach)

foreign import ccall safe "flipper/flipper.h flipper_select"
    c_flipper_select :: CString -> IO CInt

foreign import ccall safe "flipper/flipper.h flipper_attach"
    c_flipper_attach :: CInt -> CString -> IO CInt

foreign import ccall safe "flipper/flipper.h flipper_detach"
    c_flipper_detach :: CString -> IO CInt

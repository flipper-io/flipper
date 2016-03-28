module Flipper.Internal.Flipper where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Utils


-- | An endpoint via which a Flipper device may be attached.
data Endpoint = USB     -- ^ Local connection via USB.
              | Network -- ^ Remote connection via TCP/UDP.
              | FVM     -- ^ Flipper simulation via FVM.

select :: String -> IO Bool
select n = do
    n' <- newCString n
    r <- (not . toBool) <$> c_flipper_select n'
    free n'
    return r

attach :: Endpoint -> String -> IO Bool
attach e n = do
    n' <- newCString n
    r <- (not . toBool) <$> c_flipper_attach (enumEndpoint e) n'
    free n'
    return r
    where enumEndpoint USB     = 0
          enumEndpoint Network = 1
          enumEndpoint FVM     = 2

foreign import ccall safe "flipper/flipper/flipper.h flipper_select"
    c_flipper_select :: CString -> IO CInt

foreign import ccall safe "flipper/flipper/flipper.h flipper_attach"
    c_flipper_attach :: CInt -> CString -> IO CInt

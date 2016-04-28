{-# LANGUAGE BangPatterns #-}

module Flipper.Get where

import Data.Char

import Flipper.Buffer

import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

import System.IO.Unsafe

-- Refactor this to use CPS later.
newtype Get a = Get { unGet :: (Buffer -> IO (Result a)) }

data Result a = Done !Buffer a
              | WantMore Int (Get a)
              | Failure !Buffer String

instance Functor Result where
    fmap f (Done b x)     = Done b (f x)
    fmap f (WantMore l g) = WantMore l (fmap f g)
    fmap _ (Failure b s)  = Failure b s

instance Functor Get where
    fmap f (Get g) = Get $ (fmap (fmap f)) . g

instance Applicative Get where
    pure x              = Get (\b -> return (Done b x))
    (Get f) <*> (Get x) = Get $ \b -> do f' <- f b
                                         case f' of (Done b' f'')  -> do x' <- x b'
                                                                         case x' of (Done b'' x'')  -> return (Done b'' (f'' x''))
                                                                                    (WantMore l c)  -> return (WantMore l (fmap f'' c))
                                                                                    (Failure b'' e) -> return (Failure b'' e)
                                                    (WantMore l c) -> return (WantMore l (c <*> (Get x)))
                                                    (Failure b' e) -> return (Failure b' e)

instance Monad Get where
    return        = pure
    (Get x) >>= f = Get $ \b -> do x' <- x b
                                   case x' of (Done b' x'')  -> (unGet (f x'')) b'
                                              (WantMore l c) -> return (WantMore l (c >>= f))
                                              (Failure b' e) -> return (Failure b' e)

-- | Run a 'Get' that can't be fed more input.
runGet :: Get a -> Buffer -> IO (Either String a)
runGet (Get g) = (checkResult <$>) . g
    where checkResult (Done _ x)    = Right x
          checkResult (Failure _ e) = Left e
          checkResult (WantMore _ _)  = Left "runGet: parser requested more input."

-- | Run a 'Get' that may be supplied more input.
runGetSupply :: Get a -> Buffer -> (Int -> IO Buffer) -> IO (Either String a)
runGetSupply (Get g) i c = do
    r <- g i
    case r of (Done _ x)    -> return (Right x)
              (Failure _ e) -> return (Left e)
              (WantMore l n)  -> c l >>= (\b -> runGetSupply n b c)

getStorable :: Storable a => Get a
getStorable = Get g
    where g (Buffer p o l) = withForeignPtr p $ \p' -> (\v -> Done (Buffer p (o + sizeOf v) (l - sizeOf v)) v)
                         <$> peek (plusPtr (castPtr p') (fromIntegral o))

-- | Assumes ASCII encoding and presence of NULL terminator.
--getString :: Get String
--getString = Get g
--    where g (Buffer p o l) = withForeignPtr p $ \p' -> do
--                c <- peek (plusPtr (castPtr p') (fromIntegral o))
--                case c of 0 -> return $ Done (Buffer p (o + sizeOf c) (l - sizeOf c)) []
--                          _ -> if l == 1 then (c:) <$> 

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Freer.SafeIO where

import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Control.Monad.Freer.Exception
import Control.Exception


data SIO a where
  DoIO :: IO a -> SIO (Either SomeException a)

runSafeIO :: Eff '[SIO, Exc SomeException] w -> IO w
runSafeIO (Val x) = return x
runSafeIO (E u q) | Just (DoIO m) <- prj u = try m >>= runSafeIO . qApp q
runSafeIO (E u _) | Just (Exc e)  <- prj u = throw (e :: SomeException)
runSafeIO (E _ _) = error "can't happen"

safeIO :: (Member SIO r, Member (Exc SomeException) r) => IO a -> Eff r a
safeIO io = either throwError return =<< send (DoIO io)


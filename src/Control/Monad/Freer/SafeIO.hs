{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Freer.SafeIO
  ( SIO ()
  , runSafeIO
  , safeIO
  ) where

import Control.Monad.Freer (send, Member)
import Control.Monad.Freer.Internal (qApp, prj, Eff (E, Val))
import Control.Monad.Freer.Exception (Exc (Exc), throwError)
import Control.Exception (throw, SomeException, try)
import Prelude (Maybe (Just), IO, Either (), (>>=), return, (=<<), either, error, (.))

------------------------------------------------------------------------------
-- | Safe IO effect.
data SIO a where
  Safely :: IO a -> SIO (Either SomeException a)


------------------------------------------------------------------------------
-- | Interprets 'SIO' into 'IO'. There is an @Exc SomeException@ *underneath*
-- the safe IO effect here to ensure we always have it in our effect stack. Any
-- exceptions left unhandled in this effect will be rethrown in IO.
runSafeIO :: Eff '[SIO, Exc SomeException] w -> IO w
runSafeIO (Val x) = return x
runSafeIO (E u q) | Just (Safely m) <- prj u = try m >>= runSafeIO . qApp q
runSafeIO (E u _) | Just (Exc e)    <- prj u = throw (e :: SomeException)
runSafeIO (E _ _) = error "can't happen"


------------------------------------------------------------------------------
-- | Lift an 'IO' action to the 'SIO' effect.
safeIO :: (Member SIO r, Member (Exc SomeException) r) => IO a -> Eff r a
safeIO io = either throwError return =<< send (Safely io)


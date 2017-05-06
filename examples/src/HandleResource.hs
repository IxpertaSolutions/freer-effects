{-# LANGUAGE DataKinds                          #-}
{-# LANGUAGE FlexibleContexts                   #-}
{-# LANGUAGE FlexibleInstances                  #-}
{-# LANGUAGE MultiParamTypeClasses              #-}
{-# LANGUAGE RankNTypes                         #-}
{-# LANGUAGE ScopedTypeVariables                #-}
{-# LANGUAGE TypeApplications                   #-}
{-# LANGUAGE TypeFamilies                       #-}
{-# LANGUAGE TypeOperators                      #-}
{-# OPTIONS_GHC -fno-warn-orphans               #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module HandleResource where

import           Control.Exception (SomeException)
import           Control.Monad.Freer (Member, Eff ())
import           Control.Monad.Freer.Exception (Exc (..))
import           Control.Monad.Freer.SafeIO (SIO, safeIO)
import           Control.Monad.Freer.Resource (handleRegionRelay, catchSafeIOExcs, Ancestor, ResourceCtor, SafeForRegion, Resource, RegionEff, Region, unsafeWithResource, acquire)
import           System.IO (Handle)
import qualified System.IO as IO


------------------------------------------------------------------------------
-- | A 'Handle' is constructed by a 'FilePath' and an 'IO.IOMode'.
type instance ResourceCtor Handle = (FilePath, IO.IOMode)


------------------------------------------------------------------------------
-- | 'Handle's are only region-safe in SafeIO.
instance SafeForRegion Handle '[SIO, Exc SomeException]


------------------------------------------------------------------------------
-- | Example region for acquiring file 'Handle's.
fileHandleRegion :: forall r a
                  . ( Member SIO r
                    , SafeForRegion Handle r
                    , Member (Exc SomeException) r
                    )
                 => Region Handle r a
                 -> Eff r a
fileHandleRegion = handleRegionRelay (safeIO . uncurry IO.openFile)
                                     (safeIO . IO.hClose)
                                     catchSafeIOExcs


------------------------------------------------------------------------------
-- | Opens a file.
openFile :: forall r s
         . ( s ~ Ancestor 0 r
           , Member (RegionEff Handle s) r
           )
        => FilePath
        -> IO.IOMode
        -> Eff r (Resource Handle s)
openFile = curry $ acquire @Handle


------------------------------------------------------------------------------
-- | Get the contents of a file 'Handle'.
hGetContents :: (Member SIO r, Member (Exc SomeException) r)
             => Resource Handle s
             -> Eff r String
hGetContents rh = unsafeWithResource rh $ safeIO . IO.hGetContents


------------------------------------------------------------------------------
-- | Put a 'String' into a file 'Handle'.
hPutStr :: (Member SIO r, Member (Exc SomeException) r)
        => Resource Handle s
        -> String
        -> Eff r ()
hPutStr rh str = unsafeWithResource rh $ \h ->
  safeIO $ IO.hPutStr h str


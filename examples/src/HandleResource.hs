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

import           Control.Exception (SomeException, catch)
import           Control.Monad.Freer
import           Control.Monad.Freer.Exception (Exc (..), throwError)
import           Control.Monad.Freer.Internal (prj)
import           Control.Monad.Freer.Resource
import           System.IO (Handle)
import qualified System.IO as IO

------------------------------------------------------------------------------
-- | Example region for acquiring file 'Handle's.
fileHandleRegion :: forall r a
                  . ( Member IO r
                    , SafeForRegion Handle r
                    , Member (Exc SomeException) r
                    )
                 => Region Handle r a
                 -> Eff r a
fileHandleRegion = handleRegionRelay (send . uncurry IO.openFile) (send . close) handler
  where
    close :: Handle -> IO ()
    close fh = do
      catch (IO.hClose fh) (\(_ :: SomeException) -> return ())

    handler releaseAll ignore u =
      case prj u of
        Just (Exc e) -> releaseAll >> throwError (e :: SomeException)
        Nothing      -> ignore u

type instance ResourceCtor Handle = (FilePath, IO.IOMode)

instance SafeForRegion Handle r => SafeForRegion Handle (Exc SomeException ': r)
instance SafeForRegion Handle '[IO]

openFile :: forall r s
         . ( s ~ Ancestor 0 r
           , Member (RegionEff Handle s) r
           )
        => FilePath
        -> IO.IOMode
        -> Eff r (Resource Handle s)
openFile = curry $ acquire @Handle


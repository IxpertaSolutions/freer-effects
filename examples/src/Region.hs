{-# LANGUAGE DataKinds                          #-}
{-# LANGUAGE FlexibleContexts                   #-}
{-# LANGUAGE FlexibleInstances                  #-}
{-# LANGUAGE MultiParamTypeClasses              #-}
{-# LANGUAGE RankNTypes                         #-}
{-# LANGUAGE ScopedTypeVariables                #-}
{-# LANGUAGE TypeApplications                   #-}
{-# LANGUAGE TypeFamilies                       #-}
{-# LANGUAGE TypeOperators                      #-}
{-# LANGUAGE UndecidableInstances               #-}
{-# OPTIONS_GHC -fno-warn-orphans               #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Region where

import           Control.Exception
import           Control.Monad.Freer
import           Control.Monad.Freer.Resource
import           Control.Monad.Freer.SafeIO
import           Control.Monad.Freer.Exception (Exc (..))

data Crazy = Crazy deriving Eq

instance SafeForRegion Crazy '[SIO, Exc SomeException]
type instance ResourceCtor Crazy = ()

crazyRegion :: forall r a
             . ( SafeForRegion Crazy r
               , Member SIO r
               , Member (Exc SomeException) r
               )
            => Region Crazy r a
            -> Eff r a
crazyRegion = handleRegionRelay mkCrazy rmCrazy catchSafeIOExcs
  where
    mkCrazy _ = do
      safeIO $ putStrLn "acquiring crazy"
      return Crazy

    rmCrazy _ = do
      safeIO $ putStrLn "unallocating crazy"


goCrazy :: forall r s
         . ( s ~ Ancestor 0 r
           , Member (RegionEff Crazy s) r
           )
        => Eff r (Resource Crazy s)
goCrazy = acquire @Crazy ()

------------------------------------------------------------------------------

don'tReleaseOnReturn :: IO ()
don'tReleaseOnReturn = runSafeIO $ do
  crazyRegion $ do
    _ <- goCrazy
    _ <- return True
    safeIO $ putStrLn "we still have a crazy in scope :)"

{-
acquiring crazy
we still have a crazy in scope :)
unallocating crazy
-}

runFinalizerOnError :: IO ()
runFinalizerOnError = runSafeIO $ do
  crazyRegion $ do
    _ <- goCrazy
    _ <- safeIO $ throwIO Overflow
    safeIO $ putStrLn "this doesn't get run"

{-
acquiring crazy
unallocating crazy
*** Exception: arithmetic overflow
-}

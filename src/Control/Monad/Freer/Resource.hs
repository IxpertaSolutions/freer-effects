{-# LANGUAGE ConstraintKinds                    #-}
{-# LANGUAGE DataKinds                          #-}
{-# LANGUAGE FlexibleContexts                   #-}
{-# LANGUAGE FlexibleInstances                  #-}
{-# LANGUAGE GADTs                              #-}
{-# LANGUAGE PolyKinds                          #-}
{-# LANGUAGE RankNTypes                         #-}
{-# LANGUAGE ScopedTypeVariables                #-}
{-# LANGUAGE TypeFamilies                       #-}
{-# LANGUAGE TypeOperators                      #-}
{-# LANGUAGE UndecidableInstances               #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Control.Monad.Freer.Resource where

import Control.Exception
import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Control.Monad.Freer.Exception
import GHC.TypeLits (Nat, type (+), type (-), type (<=))
import Data.Proxy
import Data.List (delete)
import System.IO

data SIO a where
  DoIO :: IO a -> SIO (Either SomeException a)

type SMonadIO r = (Member SIO r, Member (Exc SomeException) r)

lIO :: SMonadIO r => IO a -> Eff r a
lIO m = send (DoIO m) >>= either throwError return

topSIO :: Eff '[SIO, Exc SomeException] w -> IO w
topSIO (Val x) = return x
topSIO (E u q) | Just (DoIO m) <- prj u = try m >>= topSIO . qApp q
topSIO (E u _) | Just (Exc e)  <- prj u = throw (e :: SomeException)
topSIO (E _ _) = error "cannot happen"


class SafeForRegion (r :: [* -> *])
instance SafeForRegion '[]
instance SafeForRegion r => SafeForRegion (SIO ': r)
instance SafeForRegion r => SafeForRegion (Exc SomeException ': r)
instance SafeForRegion r => SafeForRegion (RegionEff s ': r)
-- instance SafeForRegion r => SafeForRegion (Reader a ': r)
-- instance SafeForRegion r => SafeForRegion (State a ': r)

newtype SHandle s = SHandle Handle


-- Data constructors are not exported
data RegionEff s a where
  RENew :: FilePath -> IOMode -> RegionEff s (SHandle s)
  -- Used for duplicating regions
  REForget  :: SHandle s -> RegionEff s ()
  REAcquire :: SHandle s' -> RegionEff s (SHandle s)

type family Ancestor (n::Nat) (lst :: [* -> *]) :: * where
  Ancestor 0 (RegionEff s ': lst)     = s
  Ancestor n (RegionEff s ': lst)     = Ancestor (n-1) lst
  Ancestor n  (t ': lst)              = Ancestor n lst

newSHandle :: (SMonadIO r, s ~ Ancestor 0 r, Member (RegionEff s) r) => FilePath -> IOMode -> Eff r (SHandle s)
newSHandle = newSHandle' (Proxy::Proxy 0)

newSHandle' :: (SMonadIO r, s ~ Ancestor n r, Member (RegionEff s) r) => Proxy n -> FilePath -> IOMode -> Eff r (SHandle s)
newSHandle' _ fname fmode = send (RENew fname fmode)

type family Length (lst :: [* -> *]) :: Nat where
  Length '[] = 0
  Length (RegionEff x ': t) = 1 + (Length t)
  Length (h ': t) = Length t

data L (n::Nat) k


newRgn :: forall r a. SMonadIO r =>
          (forall s. Eff (RegionEff (L (Length r) s) ': r) a) -> Eff r a
newRgn m = loop [] m
 where
   loop :: [Handle] -> Eff (RegionEff (L (Length r) s) ': r) a -> Eff r a
   loop fhs (Val x) = close_fhs fhs >> return x
   loop fhs (E u q)  = case decomp u of
     Right (RENew fname fmode) -> do
       fh <- lIO $ openFile fname fmode -- may raise exc
       k (fh:fhs) (SHandle fh)
     Right (REForget (SHandle fh)) -> k (delete fh fhs) ()
     Right (REAcquire (SHandle fh)) ->
       k (if fh `elem` fhs then fhs else fh:fhs) (SHandle fh)
     Left  u' -> case prj u' of
       Just (Exc e) -> close_fhs fhs >> throwError (e::SomeException)
       Nothing      -> E u' (tsingleton (k fhs))
    where k s = qComp q (loop s)

       -- Close all file handles of a region
   close_fhs []  = return ()
   close_fhs fhs = send (DoIO (mapM_ close fhs)) >> return ()
   close :: Handle -> IO ()
   close fh = do
    hPutStrLn stderr $ "Closing " ++ show fh
    catch (hClose fh) (\(e::SomeException) ->
                        hPutStrLn stderr ("Error on close: " ++ show e))

type ActiveRegion s r = (Member (RegionEff s) r, SMonadIO r)

shDup :: (ActiveRegion s r, ActiveRegion s' r,
         s' ~ Ancestor n r,
         s ~ (L n1 e1), s' ~ (L n2 e2), n2 <= n1) => Proxy n -> SHandle s -> Eff r (SHandle s')
shDup _ h =
  send (REForget h) >> send (REAcquire h)



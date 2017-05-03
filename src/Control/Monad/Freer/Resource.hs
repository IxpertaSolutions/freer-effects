{-# LANGUAGE ConstraintKinds                    #-}
{-# LANGUAGE DataKinds                          #-}
{-# LANGUAGE FlexibleContexts                   #-}
{-# LANGUAGE FlexibleInstances                  #-}
{-# LANGUAGE GADTs                              #-}
{-# LANGUAGE MultiParamTypeClasses              #-}
{-# LANGUAGE PolyKinds                          #-}
{-# LANGUAGE RankNTypes                         #-}
{-# LANGUAGE ScopedTypeVariables                #-}
{-# LANGUAGE TypeApplications                   #-}
{-# LANGUAGE TypeFamilies                       #-}
{-# LANGUAGE TypeOperators                      #-}
{-# LANGUAGE UndecidableInstances               #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Control.Monad.Freer.Resource
  ( SafeForRegion
  , Resource ()
  , unsafeWithResource
  , ResourceCtor
  , catchNothing
  , acquire
  , acquire'
  , region
  , give
  ) where

import Data.Bool (bool)
import Control.Exception
import Control.Monad.Freer
import Control.Monad.Freer.Internal
import Control.Monad.Freer.Exception
import GHC.TypeLits (Nat, type (+), type (-), type (<=))
import Data.Proxy
import Data.List (delete)
import System.IO


class SafeForRegion res (r :: [* -> *])
instance SafeForRegion res '[]
instance SafeForRegion Handle r => SafeForRegion Handle (Exc SomeException ': r)
instance SafeForRegion Handle '[IO]
instance SafeForRegion res r => SafeForRegion res (RegionEff res s ': r)
-- instance SafeForRegion r => SafeForRegion (Reader a ': r)
-- instance SafeForRegion r => SafeForRegion (State a ': r)

newtype Resource res s = Resource res


-- TODO(sandy): how can we existentialize 'res' so it can't escape?
unsafeWithResource :: Resource res s -> (res -> a) -> a
unsafeWithResource (Resource r) f = f r


type family ResourceCtor res

type instance ResourceCtor Handle = (FilePath, IOMode)

-- Data constructors are not exported
data RegionEff res s a where
  RENew :: ResourceCtor res -> RegionEff res s (Resource res s)
  -- Used for duplicating regions
  REForget  :: Resource res s -> RegionEff res s ()
  REAcquire :: Resource res s' -> RegionEff res s (Resource res s)

type family Ancestor (n::Nat) (lst :: [* -> *]) :: * where
  Ancestor 0 (RegionEff res s ': lst)     = s
  Ancestor n (RegionEff res s ': lst)     = Ancestor (n-1) lst
  Ancestor n  (t ': lst)              = Ancestor n lst

acquire :: forall res r s
         . ( s ~ Ancestor 0 r
           , Member (RegionEff res s) r
           )
        => ResourceCtor res
        -> Eff r (Resource res s)
acquire = acquire' $ Proxy @0

acquire' :: (s ~ Ancestor n r, Member (RegionEff res s) r)
            => Proxy n
            -> ResourceCtor res
            -> Eff r (Resource res s)
acquire' _ ctor = send $ RENew ctor

type family Length (lst :: [* -> *]) :: Nat where
  Length '[] = 0
  Length (RegionEff res x ': t) = 1 + (Length t)
  Length (h ': t) = Length t

data L (n::Nat) k


handleRegionRelay :: forall res effs a
                   . (SafeForRegion res effs, Eq res)
                  => (ResourceCtor res -> Eff effs res)
                  -> (res -> Eff effs ())
                  -> (forall (b :: *). Eff effs ()
                             -> (Union effs b -> Eff effs a)
                             -> Union effs b
                             -> Eff effs a
                     )
                  -> (forall s. Eff (RegionEff res (L (Length effs) s) ': effs) a)
                  -> Eff effs a
handleRegionRelay acquireM releaseM catchM = loop []
  where
    releaseAll = mapM_ releaseM

    loop :: [res] -> Eff (RegionEff res (L (Length effs) s) ': effs) a -> Eff effs a
    loop rs (Val x) = releaseAll rs >> return x
    loop rs (E u q) =
      case decomp u of
        Right (RENew ctor) -> do
          r <- acquireM ctor
          k (r : rs) $ Resource r

        Right (REForget (Resource r)) ->
          k (delete r rs) ()

        Right (REAcquire (Resource r)) ->
          k (bool (r : rs) rs $ elem r rs) $ Resource r

        Left u' ->
          catchM (releaseAll rs) ignore u'

      where
        ignore u' = E u' . tsingleton $ k rs
        k s = qComp q (loop s)


catchNothing :: Eff effs ()
             -> (Union effs b -> Eff effs a)
             -> Union effs b
             -> Eff effs a
catchNothing = const id



region :: forall r a
        . ( Member IO r
          , SafeForRegion Handle r
          , Member (Exc SomeException) r
          )
       => (forall s. Eff (RegionEff Handle (L (Length r) s) ': r) a)
       -> Eff r a
region = handleRegionRelay (send . uncurry openFile) (send . close) handler
  where
    close :: Handle -> IO ()
    close fh = do
      hPutStrLn stderr $ "Closing " ++ show fh
      catch (hClose fh) (\(e::SomeException) ->
                          hPutStrLn stderr ("Error on close: " ++ show e))

    handler releaseAll ignore u =
      case prj u of
        Just (Exc e) -> releaseAll >> throwError (e :: SomeException)
        Nothing -> ignore u


-- test :: Eff '[Exc SomeException, IO] String
-- test = region $ do
--         _ <- acquire @Handle ("hello", ReadMode)
--         return "hello"




type ActiveRegion res s r = (Member (RegionEff res s) r)

give :: ( ActiveRegion res s r
        , ActiveRegion res s' r
        , s' ~ Ancestor n r
        , s ~ L n1 e1
        , s' ~ L n2 e2
        , n2 <= n1
        )
     => Proxy n
     -> Resource res s
     -> Eff r (Resource res s')
give _ h = send (REForget h) >> send (REAcquire h)


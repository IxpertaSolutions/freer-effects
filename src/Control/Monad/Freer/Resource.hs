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
  , handleRegionRelay
  , fileHandleRegion
  , give
  , thisRegion
  , parentRegion
  , gparentRegion
  ) where

import Control.Exception (SomeException, catch)
import Control.Monad.Freer
import Control.Monad.Freer.Exception (Exc (..), throwError)
import Control.Monad.Freer.Internal (Union, Eff (..), qComp, tsingleton, prj, decomp)
import Data.Bool (bool)
import Data.List (delete)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (Nat, type (+), type (-), type (<=))
import System.IO


------------------------------------------------------------------------------
-- | Instances of this typeclass describe which effects 'r' are allowed to be
-- present inside of a fileHandleRegion for 'res'.
class SafeForRegion res (r :: [* -> *])

instance SafeForRegion res '[]
instance SafeForRegion res r => SafeForRegion res (RegionEff res s ': r)

instance SafeForRegion Handle r => SafeForRegion Handle (Exc SomeException ': r)
instance SafeForRegion Handle '[IO]
-- instance SafeForRegion r => SafeForRegion (Reader a ': r)
-- instance SafeForRegion r => SafeForRegion (State a ': r)


------------------------------------------------------------------------------
-- | A wrapper around a 'res', with an eigenvariable to prevent this type from
-- exiting the fileHandleRegion its lifetime is scoped by.
newtype Resource res s = Resource res


------------------------------------------------------------------------------
-- | Helper function to allow library writers to work with 'Resource's. End
-- users must never use this function, as its misuse can leak resources from
-- their scoping regions.
--
-- TODO(sandy): how can we existentialize 'res' so it can't escape?
unsafeWithResource :: Resource res s -> (forall b. (res ~ b) => b -> a) -> a
unsafeWithResource (Resource r) f = f r


------------------------------------------------------------------------------
-- | Type family for describing the data necessary to construct a request to
-- acquire a 'res'.
type family ResourceCtor res

type instance ResourceCtor Handle = (FilePath, IOMode)


------------------------------------------------------------------------------
-- | Underlying effect for managing regions.
data RegionEff res s a where
  -- | Create a new 'res'.
  RENew :: ResourceCtor res -> RegionEff res s (Resource res s)
  -- | Forget about a 'res' (because it's been 'give'n to another fileHandleRegion).
  REForget  :: Resource res s -> RegionEff res s ()
  -- | Acquire a 'res' from another fileHandleRegion.
  REAcquire :: Resource res s' -> RegionEff res s (Resource res s)


------------------------------------------------------------------------------
-- | Finds the 'n'th occurrence of a RegionEff, which must exist.
type family Ancestor (n::Nat) (lst :: [* -> *]) :: * where
  Ancestor 0 (RegionEff res s ': lst) = s
  Ancestor n (RegionEff res s ': lst) = Ancestor (n - 1) lst
  Ancestor n  (t ': lst)              = Ancestor n lst


------------------------------------------------------------------------------
-- | Helper value to describe 'acquire'ing a resource in this fileHandleRegion.
thisRegion :: Proxy 0
thisRegion = Proxy


------------------------------------------------------------------------------
-- | Helper value to describe 'acquire'ing a resource in the parent fileHandleRegion.
parentRegion :: Proxy 1
parentRegion = Proxy


------------------------------------------------------------------------------
-- | Helper value to describe 'acquire'ing a resource in the grandparent fileHandleRegion.
gparentRegion :: Proxy 2
gparentRegion = Proxy


------------------------------------------------------------------------------
-- | Acquire a 'res' scoped in the current fileHandleRegion.
acquire :: forall res r s
         . ( s ~ Ancestor 0 r
           , Member (RegionEff res s) r
           )
        => ResourceCtor res
        -> Eff r (Resource res s)
acquire = acquire' thisRegion


------------------------------------------------------------------------------
-- | Acquire a 'res' scoped in an ancestor fileHandleRegion.
acquire' :: ( s ~ Ancestor n r
            , Member (RegionEff res s) r
            )
         => Proxy n
         -> ResourceCtor res
         -> Eff r (Resource res s)
acquire' _ ctor = send $ RENew ctor


------------------------------------------------------------------------------
-- | Count how many 'RegionEff's there are in an effect stack.
type family Length (lst :: [* -> *]) :: Nat where
  Length '[] = 0
  Length (RegionEff res x ': t) = 1 + (Length t)
  Length (h ': t) = Length t


------------------------------------------------------------------------------
-- | Internal type to create eigenvariables around, in order to protect
-- resources from exiting their fileHandleRegion scope.
data L (n :: Nat) k


------------------------------------------------------------------------------
-- | Helper function to build fileHandleRegion constructs for resources of type 'res'.
handleRegionRelay :: forall res effs a
                   . ( SafeForRegion res effs
                     , Eq res
                     )
                  => -- | Strategy to acquire a 'res'.
                     (ResourceCtor res -> Eff effs res)
                     -- | Strategy to release a 'res'.
                  -> (res -> Eff effs ())
                     -- | Strategy for catching other effects.
                  -> (forall (b :: *)
                         . -- | Action to release all resources.
                           Eff effs ()
                           -- | Action to ignore this effect.
                        -> (Union effs b -> Eff effs a)
                           -- | The effect being caught.
                        -> Union effs b
                        -> Eff effs a
                     )
                     -- | A fileHandleRegion in which we can allocate 'res's.
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


------------------------------------------------------------------------------
-- | Combinator to be used with 'handleRegionRelay' describing that no extra
-- effects need being caught.
catchNothing :: Eff effs ()
             -> (Union effs b -> Eff effs a)
             -> Union effs b
             -> Eff effs a
catchNothing = const id


------------------------------------------------------------------------------
-- | Example region for acquiring file 'Handle's.
fileHandleRegion :: forall r a
                  . ( Member IO r
                    , SafeForRegion Handle r
                    , Member (Exc SomeException) r
                    )
                 => (forall s. Eff (RegionEff Handle (L (Length r) s) ': r) a)
                 -> Eff r a
fileHandleRegion = handleRegionRelay (send . uncurry openFile) (send . close) handler
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


------------------------------------------------------------------------------
-- | Type alias to describe that 's' is a region currently in scope.
type ActiveRegion res s r = Member (RegionEff res s) r


------------------------------------------------------------------------------
-- | Transfer ownership of a resource to a parent region. Useful for resources
-- with conditional lifetimes.
give :: ( ActiveRegion res s r
        , ActiveRegion res s' r
        , s' ~ Ancestor n r
        , s  ~ L n1 e1
        , s' ~ L n2 e2
        , n2 <= n1
        )
     => Proxy n
     -> Resource res s
     -> Eff r (Resource res s')
give _ h = send (REForget h) >> send (REAcquire h)


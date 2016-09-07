{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}

{-|
Module      : Data.Open.Union
Description : Open unions (type-indexed co-products) for extensible effects.
Copyright   : Alej Cabrera 2015
License     : BSD-3
Maintainer  : cpp.cabrera@gmail.com
Stability   : experimental
Portability : POSIX

This implementation relies on _closed_ type families added to GHC 7.8. It has NO overlapping instances and NO Typeable. This version preserves constant-time injection/projection by emulating Typeable.

Using <http://okmij.org/ftp/Haskell/extensible/OpenUnion5.hs> as a starting point.
-}

module Data.Open.Union (
  Union,
  decomp,
  weaken,
  Member(..),
  Members
) where

import GHC.Exts
import GHC.TypeLits
import Unsafe.Coerce

data Union :: [* -> *] -> * -> * where
    Union :: !Int -> Any -> Union r v

{-# INLINE decomp #-}
decomp :: Union (t ': r) v -> Either (Union r v) (t v)
decomp (Union 0 v) = Right $ unsafeCoerce v
decomp (Union n v) = Left $ Union (n - 1) v

{-# INLINE weaken #-}
weaken :: Union r v -> Union (any ': r) v
weaken (Union n v) = Union (n + 1) v

class KnownNat (FindElem t r) => Member t r where
    inj :: t v -> Union r v
    prj :: Union r v -> Maybe (t v)

instance KnownNat (FindElem t r) => Member t r where
    {-# INLINE inj #-}
    inj v = Union (fromInteger $ natVal' (proxy# :: Proxy# (FindElem t r))) (unsafeCoerce v)

    {-# INLINE prj #-}
    prj (Union i v) = if fromInteger (natVal' (proxy# :: Proxy# (FindElem t r))) == i then Just $ unsafeCoerce v else Nothing

type family FindElem t r where
    FindElem t (t ': _) = 0
    FindElem t (_ ': r) = 1 + FindElem t r

type family Members m r :: Constraint where
  Members (t ': c) r = (Member t r, Members c r)
  Members '[] r = ()

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:       Control.Monad.Freer.Writer
-- Description:  Composable Writer effects.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- 'Writer' effects, for writing\/appending values (line count, list of
-- messages, etc.) to an output. Current value of 'Writer' effect output is not
-- accessible to the computation.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Writer
    ( Writer(..)
    , tell
    , runWriter
    , handleWriter
    , ignoreWriter
    )
  where

import Control.Applicative (pure,(*>))
import Control.Arrow (second)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Monoid (Monoid, (<>), mempty)

import Control.Monad.Freer.Internal (Eff, Member, handleRelay, send)


-- | Writer effects - send outputs to an effect environment.
data Writer w a where
    Writer :: w -> Writer w ()

-- | Send a change to the attached environment.
tell :: Member (Writer w) effs => w -> Eff effs ()
tell w = send $ Writer w

-- | Simple handler for 'Writer' effects.
runWriter :: Monoid w => Eff (Writer w ': effs) a -> Eff effs (a, w)
runWriter = handleRelay (\a -> pure (a, mempty)) $ \(Writer w) k ->
    second (w <>) <$> k ()

-- | Process written values as they happen - useful for logging while interpreting an
-- application instead of gathering all values:
-- 
-- >  handleWriter print :: (Member IO effs, Show a) => Eff (Writer a ': effs) b -> Eff effs b
--
-- This allows for multiple writers to be handled differently:
--
-- @
-- newtype Debug = Debug String
-- newtype Info  = Info String
--
-- ignoreDebug :: Member IO effs => Eff (Writer Debug : effs) a -> Eff effs a
-- ignoreDebug = handleWriter (const (pure ()))
--
-- printInfo :: Member IO effs => Eff (Writer Info : effs) a -> Eff effs a
-- printInfo = handleWriter (\(Info s) -> putStrLn s)
-- @

handleWriter :: (Member m effs) => (w -> m b) -> Eff (Writer w ': effs) a -> Eff effs a
handleWriter prnt = handleRelay pure (\(Writer w) k -> send (prnt w) *> k ())

-- | Ignore written values of a particular type.
ignoreWriter :: proxy w -> Eff (Writer w ': effs) a -> Eff effs a
ignoreWriter _ = handleRelay pure (\(Writer _) k -> k ())

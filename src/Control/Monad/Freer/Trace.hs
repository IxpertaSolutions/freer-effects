{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:       Control.Monad.Freer.Trace
-- Description:  Composable Trace effects.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Composable handler for 'Trace' effects. Trace allows one to debug the
-- operation of sequences of effects by outputing to the console.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Trace
    ( Trace(..)
    , trace
    , runTrace
    , runTraceIO
    , runTraceSilent
    )
  where

import Control.Monad ((>>=), return)
import Data.Either (Either(Left, Right))
import Data.Function ((.), const)
import Data.String (String)
import System.IO (IO, putStrLn)

import Control.Monad.Freer.Internal
    (Eff(E, Val), Member, decomp, qApp, send, tsingleton)


-- | A Trace effect.
data Trace s a where
    Trace :: s -> Trace s ()

-- | Printing a string in a trace.
trace :: Member (Trace s) effs => s -> Eff effs ()
trace = send . Trace

-- | Generic runner for 'Trace' effect.
runTrace
    :: (s -> Eff effs ())
    -- ^ Function to trace 's' in terms of effect stack without 'Trace'.
    -> Eff (Trace s ': effs) a -> Eff effs a
runTrace _ (Val x) = return x
runTrace f (E u q) = case decomp u of
    Right (Trace s) -> f s >>= runTrace f . qApp q
    Left u' -> E u' (tsingleton (runTrace f . qApp q))

-- | Simple runner for 'Trace String'.
runTraceIO :: Member IO effs => Eff (Trace String ': effs) a -> Eff effs a
runTraceIO = runTrace (send . putStrLn)

-- | Ignore traces.
runTraceSilent
    :: forall proxy effs s a
    . proxy s
    -- ^ Proxy for what traces to throw away.
    -> Eff (Trace s ': effs) a -> Eff effs a
runTraceSilent p = runTrace (f p)
  where
    f :: proxy s -> s -> Eff effs ()
    f _ = const (return ())

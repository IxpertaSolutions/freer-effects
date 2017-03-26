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
-- Composable handler for 'Trace' effects. Trace allows one to debug/log the
-- operation of sequences of effects.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Trace
    (
    -- * Trace Effect
      Trace(..)

    -- * Trace Operations
    , trace

    -- * Trace Handlers
    , runTrace
    , runTraceIO
    , runTraceSilent

    -- * Example: Simple Logging Facility
    -- $simpleLoggingFacility
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

-- | Trace a value.
trace :: Member (Trace s) effs => s -> Eff effs ()
trace = send . Trace

-- | Generic handler for 'Trace' effect.
runTrace
    :: (s -> Eff effs ())
    -- ^ Function to trace @s@ in terms of effect stack with 'Trace' popped.
    -> Eff (Trace s ': effs) a -> Eff effs a
runTrace _ (Val x) = return x
runTrace f (E u q) = case decomp u of
    Right (Trace s) -> f s >>= runTrace f . qApp q
    Left u' -> E u' (tsingleton (runTrace f . qApp q))

-- | Simple handler for 'Trace' 'String' that just writes it to stdout.
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

-- $simpleLoggingFacility
--
-- In this example the 'Trace' effect is used to provide logging facility.
-- Handler @runLogger@ provides user with ability to filter based
-- on @LogLevel@ and is able to log to any 'Handle' provided.
--
-- As a simple exercise one can modify @runLogger@ handler so that it
-- prefixes each log line with timestamp.
--
-- > {-# LANGUAGE DataKinds #-}
-- > {-# LANGUAGE ExistentialQuantification #-}
-- > {-# LANGUAGE FlexibleContexts #-}
-- > {-# LANGUAGE RecordWildCards #-}
-- > {-# LANGUAGE TypeOperators #-}
-- > module SimpleLogging where
-- >
-- > import Control.Monad (when)
-- > import System.IO (Handle, hPrint)
-- >
-- > import Control.Monad.Freer
-- > import Control.Monad.Freer.Trace
-- >
-- > data LogLevel = Debug | Info | Warn | Error
-- >   deriving (Eq, Show, Ord)
-- >
-- > data Log = forall a . Show a => Log
-- >     { level :: LogLevel
-- >     , item :: a
-- >     }
-- >
-- > type Logger = Trace Log
-- >
-- > newtype Msg = Msg String
-- >
-- > instance Show Msg where
-- >     show (Msg s) = s
-- >
-- > debug :: (Member Logger effs, Show a) => a -> Eff effs ()
-- > debug = trace . Log Debug
-- >
-- > info :: (Member Logger effs, Show a) => a -> Eff effs ()
-- > info = trace . Log Info
-- >
-- > -- ... warn, error
-- >
-- > debugMsg :: Member Logger effs => String -> Eff effs ()
-- > debugMsg = debug . Msg
-- >
-- > -- ... infoMsg, warnMsg, errorMsg
-- >
-- > runLogger
-- >     :: Member IO effs
-- >     => Handle
-- >     -> LogLevel
-- >     -> Eff (Trace Log ': effs) a -> Eff effs a
-- > runLogger h l = runTrace f
-- >   where
-- >     f Log{..} = when (l <= level) . send $ hPrint h item

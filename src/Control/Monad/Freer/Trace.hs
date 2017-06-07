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

    -- * Example 1: Simple Logging Facility
    -- $simpleLoggingFacility

    -- * Example 2: Debugging Pure Functions
    -- $debuggingPureFunctions
    )
  where

import Control.Applicative (pure)
import Control.Monad ((>>=))
import Data.Function ((.), const)
import Data.String (String)
import System.IO (IO, hPutStrLn, stderr)

import Control.Monad.Freer.Internal (Arr, Eff, Member, handleRelay, send)


-- | A Trace effect.
data Trace s a where
    Trace :: s -> Trace s ()

-- | Trace a value.
trace :: Member (Trace s) effs => s -> Eff effs ()
trace = send . Trace

-- | Generic handler for 'Trace' effect.
runTrace
   :: forall s effs a
   .  (s -> Eff effs ())
   -- ^ Function to trace @s@ in terms of effect stack with 'Trace' popped.
   -> Eff (Trace s ': effs) a -> Eff effs a
runTrace f = handleRelay pure handle
  where
    handle :: Trace s v -> Arr effs v a -> Eff effs a
    handle (Trace s) = (f s >>=)

-- | Simple handler for 'Trace' 'String' that just writes it to stdout.
runTraceIO :: Member IO effs => Eff (Trace String ': effs) a -> Eff effs a
runTraceIO = runTrace (send . hPutStrLn stderr)

-- | Ignore traces.
runTraceSilent
    :: forall proxy effs s a
    . proxy s
    -- ^ Proxy for what traces to throw away.
    -> Eff (Trace s ': effs) a -> Eff effs a
runTraceSilent p = runTrace (f p)
  where
    f :: proxy s -> s -> Eff effs ()
    f _ = const (pure ())

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
--

-- $debuggingPureFunctions
--
-- Even happened to you that you needed to trace pure function?
-- Write your function using 'Trace' effect and then use 'runTraceSilent'.
--
-- Using code from previous example (for complete example look into @examples@
-- directory in the source tree):
--
-- > data FactState = FactState
-- >     { bits :: Integer
-- >     , currentValue :: Integer
-- >     }
-- >   deriving Show
-- >
-- > _bits :: (Integer -> Integer) -> FactState -> FactState
-- > _bits f s@FactState{..} = s{bits=f bits}
-- >
-- > _currentValue :: (Integer -> Integer) -> FactState -> FactState
-- > _currentValue f s@FactState{..} = s{currentValue=f currentValue}
-- >
-- > example :: IO ()
-- > example = do
-- >     forM_ [minBound..maxBound] $ \ ll -> do
-- >         putStrLn $ "Running `factDebug 10` with LogLevel = " <> show ll
-- >         void . runM . runLogger ll $ factDebug 10
-- >         putStrLn "Done...\n\n"
-- >     putStrLn "Fact as a pure function: "
-- >     print $ fact 10
-- >
-- > fact :: Integer -> Integer
-- > fact = purify . factDebug
-- >   where
-- >     purify = run . runTraceSilent (Proxy :: Proxy LogItem)
-- >
-- > factDebug :: Member Logger effs => Integer -> Eff effs Integer
-- > factDebug n = do
-- >     val@FactState{..} <- execState (factDebug' n) (FactState 8 1)
-- >     info (Msg "Result", val)
-- >     pure currentValue
-- >
-- > factDebug' :: Members [State FactState, Logger] effs => Integer -> Eff effs ()
-- > factDebug' n = forM_ [1..n] $ \m -> do
-- >     modify $ _currentValue (m*)
-- >     FactState{..} <- get
-- >     debug (Msg "current step", m, currentValue)
-- >     when (maxFromBits bits < currentValue) $ do
-- >         warnMsg $ "Value too large for " <> show bits <> " bit store"
-- >         modify $ _bits (2*)
-- >   where
-- >     maxFromBits b = 2^(b - 1) - 1

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
module Trace (module Trace) where

import Prelude (Bounded, Enum, Integer, (*), (-), (^), maxBound, minBound)

import Control.Applicative (pure)
import Control.Monad (forM_, void, when)
import Data.Eq (Eq)
import Data.Function (($), (.))
import Data.Monoid ((<>))
import Data.Ord (Ord, (<), (<=))
import Data.Proxy (Proxy(Proxy))
import Data.String (String)
import System.IO (IO, print, putStrLn)
import Text.Show (Show(show))

import Control.Monad.Freer (Eff, Member, Members, run, runM, send)
import Control.Monad.Freer.State (State, execState, get, modify)
import Control.Monad.Freer.Trace (Trace, runTrace, runTraceSilent, trace)


data LogLevel = Debug | Info | Warn | Error
  deriving (Bounded, Enum, Eq, Ord, Show)

data LogItem = forall a . Show a => LogItem
    { level :: LogLevel
    , item :: a
    }

type Logger = Trace LogItem

newtype Msg = Msg String

instance Show Msg where
    show (Msg s) = s

debug :: (Member Logger effs, Show a) => a -> Eff effs ()
debug = trace . LogItem Debug

info :: (Member Logger effs, Show a) => a -> Eff effs ()
info = trace . LogItem Info

warn :: (Member Logger effs, Show a) => a -> Eff effs ()
warn = trace . LogItem Warn

debugMsg :: Member Logger effs => String -> Eff effs ()
debugMsg = debug . Msg

infoMsg :: Member Logger effs => String -> Eff effs ()
infoMsg = info . Msg

warnMsg :: Member Logger effs => String -> Eff effs ()
warnMsg = warn . Msg

runLogger
    :: Member IO effs
    => LogLevel -> Eff (Logger ': effs) a -> Eff effs a
runLogger l = runTrace f
  where
    f LogItem{..} = when (l <= level) . send $ print item

data FactState = FactState
    { bits :: Integer
    , currentValue :: Integer
    }
  deriving Show

_bits :: (Integer -> Integer) -> FactState -> FactState
_bits f s@FactState{..} = s{bits=f bits}

_currentValue :: (Integer -> Integer) -> FactState -> FactState
_currentValue f s@FactState{..} = s{currentValue=f currentValue}

example :: IO ()
example = do
    forM_ [minBound..maxBound] $ \ ll -> do
        putStrLn $ "Running `factDebug 10` with LogLevel = " <> show ll
        void . runM . runLogger ll $ factDebug 10
        putStrLn "Done...\n\n"
    putStrLn "Fact as a pure function: "
    print $ fact 10

fact :: Integer -> Integer
fact = purify . factDebug
  where
    purify = run . runTraceSilent (Proxy :: Proxy LogItem)

factDebug :: Member Logger effs => Integer -> Eff effs Integer
factDebug n = do
    val@FactState{..} <- execState (factDebug' n) (FactState 8 1)
    info (Msg "Result", val)
    pure currentValue

factDebug' :: Members [State FactState, Logger] effs => Integer -> Eff effs ()
factDebug' n = forM_ [1..n] $ \m -> do
    modify $ _currentValue (m*)
    FactState{..} <- get
    debug (Msg "current step", m, currentValue)
    when (maxFromBits bits < currentValue) $ do
        warnMsg $ "Value too large for " <> show bits <> " bit store"
        modify $ _bits (2*)
  where
    maxFromBits b = 2^(b - 1) - 1

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
-- |
-- Module:       Control.Monad.Freer.State
-- Description:  State effects, for state-carrying computations.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Composable handler for 'State' effects. Handy for passing an updatable state
-- through a computation.
--
-- Some computations may not require the full power of 'State' effect:
--
-- * For a read-only state, see "Control.Monad.Freer.Reader".
-- * To accumulate a value without using it on the way, see
--   "Control.Monad.Freer.Writer".
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.State
    (
    -- * State Effect
      State(..)

    -- * State Operations
    , get
    , put
    , modify
    , modify'

    -- * State Handlers
    , runState
    , evalState
    , execState

    -- * State Utilities
    , transactionState
    , handleState
    )
  where

import Control.Monad ((>>), (>>=), return, void)
import Data.Either (Either(Left, Right))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(Just))
import Data.Proxy (Proxy)
import Data.Tuple (fst, snd)
import Data.Function (($),(.))


import Control.Monad.Freer.Internal
    ( Eff(E, Val)
    , Member
    , Union
    , decomp
    , prj
    , qApp
    , qComp
    , send
    , tsingleton
    , handleRelay
    , send
    )


--------------------------------------------------------------------------------
                         -- State, strict --
--------------------------------------------------------------------------------

-- | Strict 'State' effects: one can either 'Get' values, 'Put' them, or 'Modify' them.
--
-- The inclusion of Modify allows for atomic modification when using 'handleState'.
data State s a where
    Get :: State s s
    Put :: !s -> State s ()
    Modify :: (s -> (s,a)) -> State s (s,a)

-- | Retrieve the current value of the state of type @s :: *@.
get :: Member (State s) effs => Eff effs s
get = send Get

-- | Set the current state to a specified value of type @s :: *@.
put :: Member (State s) effs => s -> Eff effs ()
put s = send (Put s)

-- | Modify the current state of type @s :: *@ using provided function
-- @(s -> s)@.
modify :: Member (State s) effs => (s -> s) -> Eff effs ()
modify f = void . send . Modify $ \s -> (f s,())

-- | Atomically modify the current state of type @s :: *@ using
-- provided function @(s -> (s,b))@ returning the new state and
-- an associated value.
modify' :: Member (State s) effs => (s -> (s,b)) -> Eff effs (s,b)
modify' f = send (Modify f)


-- | Handler for 'State' effects.
runState :: Eff (State s ': effs) a -> s -> Eff effs (a, s)
runState (Val x) s = return (x, s)
runState (E u q) s = case decomp u of
    Right Get      -> runState (qApp q s) s
    Right (Put s') -> runState (qApp q ()) s'
    Right (Modify f) -> let res@(s',_) = f s in runState (qApp q res) s'
    Left  u'       -> E u' (tsingleton (\x -> runState (qApp q x) s))

-- | Run a 'State' effect, returning only the final state.
execState :: Eff (State s ': effs) a -> s -> Eff effs s
execState st s = snd <$> runState st s

-- | Run a State effect, discarding the final state.
evalState :: Eff (State s ': effs) a -> s -> Eff effs a
evalState st s = fst <$> runState st s

-- | An encapsulated State handler, for transactional semantics. The global
-- state is updated only if the 'transactionState' finished successfully.
transactionState
    :: forall s effs a
    .  Member (State s) effs
    => Proxy s
    -> Eff effs a
    -> Eff effs a
transactionState _ m = do s <- get; loop s m
  where
    loop :: s -> Eff effs a -> Eff effs a
    loop s (Val x)                = put s >> return x
    loop s (E (u :: Union r b) q) = case prj u :: Maybe (State s b) of
        Just Get      -> loop s (qApp q s)
        Just (Put s') -> loop s'(qApp q ())
        Just (Modify f) -> let res@(s',_) = f s in loop s' (qApp q res)
        _             -> E u (tsingleton k) where k = qComp q (loop s)

-- | Provide explicit state handlers in terms of another effect, such as IO.
--
-- For example, you can turn a stateful computation into one which works on shared
-- state concurrently.
--
-- @
-- main = do
--     ref <- newIORef True
--     let getRef = readIORef ref
--         setRef = writeIORef ref
--         modRef = atomicModifyIORef ref
--     forkIO $ runM . handleState getRef setRef modRef $ statefulComputation1
--     runM . handleState getRef setRef modRef $ statefulComputation2
-- @
handleState :: Member m effs => m s -> (s -> m ()) -> (forall b. (s -> (s,b)) -> m (s,b)) -> Eff (State s ': effs) a -> Eff effs a
handleState gt st md = handleRelay return (\x k -> case x of
    Get      -> send gt     >>= k
    Put s    -> send (st s) >>= k
    Modify f -> send (md f) >>= k
    )

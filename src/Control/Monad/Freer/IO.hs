{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:       Control.Monad.Freer.IO
-- Description:  Convenience interpreter for lifting IO to MonadIO
-- Copyright:    (c) 2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    experimental
-- Portability:  GHC specific language extensions.

module Control.Monad.Freer.IO where

import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Monad.Freer ( Eff, Member, runNat )
import Prelude ( IO )

-- | Interpret IO into a MonadIO lower down the stack.
--
-- You will likely need to type-apply this to your specific 'm'.
runIOInMonadIO :: forall m effs a . (MonadIO m, Member m effs)
               => Eff (IO ': effs) a
               -> Eff effs a
runIOInMonadIO = runNat @m liftIO

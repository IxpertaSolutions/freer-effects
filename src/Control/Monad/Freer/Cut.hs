{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE Safe #-}
-- |
-- Module:       Control.Monad.Freer.Cut
-- Description:  An implementation of logical Cut.
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.
-- License:      BSD3
-- Maintainer:   ixcom-core@ixperta.com
-- Stability:    broken
-- Portability:  GHC specific language extensions.
--
-- Composable handler for logical Cut effects. Implemented in terms of 'Exc'
-- effect.
--
-- Using <http://okmij.org/ftp/Haskell/extensible/Eff1.hs> as a starting point.
module Control.Monad.Freer.Cut
    ( CutFalse(..)
    , cutFalse
--  , call
    )
  where

-- import Control.Monad
import Control.Monad.Freer.Exception (Exc, throwError)
import Control.Monad.Freer.Internal (Eff, Member)


data CutFalse = CutFalse
-- data Choose a b = Choose [a] b

-- | Implementation of logical Cut using Exc effects.
cutFalse :: Member (Exc CutFalse) r => Eff r a
cutFalse = throwError CutFalse

{-
call :: Member (Exc CutFalse) r => Eff (Exc CutFalse ': r) a -> Eff r a
call m = loop [] m where
 loop jq (Val x) = return x `mplus` next jq          -- (C2)
 loop jq (E u q) = case decomp u of
    Right (Exc CutFalse) -> mzero  -- drop jq (F2)
    Left u -> check jq u

 check jq u | Just (Choose [] _) <- prj u  = next jq  -- (C1)
 check jq u | Just (Choose [x] k) <- prj u = loop jq (k x)  -- (C3), optim
 check jq u | Just (Choose lst k) <- prj u = next $ map k lst ++ jq -- (C3)
 check jq u = send (\k -> fmap k u) >>= loop jq      -- (C4)

 next []    = mzero
 next (h:t) = loop t h
-}

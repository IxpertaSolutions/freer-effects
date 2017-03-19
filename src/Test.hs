{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}

module Test where

import Control.Monad.Freer.TH

data MyEffect s a where
  WhatWhat :: s -> MyEffect s ()
  HowHow :: Int -> Bool -> s -> MyEffect s s


makeFreer ''MyEffect

-- whatWhat :: (Member (MyEffect s) r) => s -> Eff r ()
-- howHow :: (Member (MyEffect s) r) => Int -> Bool -> s -> Eff r s

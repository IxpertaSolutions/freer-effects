{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}

module Test where


import Control.Monad.Freer.TH
import Control.Monad.Freer

data MyEffect s a where
  WhatWhat :: s -> MyEffect s ()
  HowHow :: Int -> Bool -> s -> MyEffect s s


liftInst ''MyEffect

-- whatWhat :: (Member (MyEffect s) r) => s -> Eff r ()
-- howHow :: (Member (MyEffect s) r) => Int -> Bool -> s -> Eff r s

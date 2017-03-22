{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE IncoherentInstances #-}
module Tests.Trace (tests)
  where

import Control.Monad (mapM_)
import Data.Function (($), (.))
import Data.Int (Int)
import Data.Tuple (snd)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

import Control.Monad.Freer (Eff, Member, run)
import Control.Monad.Freer.Trace (Trace, runTrace, trace)
import Control.Monad.Freer.Writer (Writer, runWriter, tell)


tests :: TestTree
tests = testGroup "Trace tests"
    [ testCase "Trace as Writer"
        $ exampleRunned @?= [1..10]
    ]

type IntTrace = Trace Int

example :: Member IntTrace r => Eff r ()
example = mapM_ trace [1..10::Int]

exampleRunned :: [Int]
exampleRunned = snd $ run $ runWriter $ runTrace tracer example
  where
    tracer :: Member (Writer [Int]) effs => Int -> Eff effs ()
    tracer = tell . (:[])

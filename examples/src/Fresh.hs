{-# LANGUAGE NoImplicitPrelude #-}
module Fresh (module Fresh) where

import Data.Function (($), (.), flip)
import Data.Monoid ((<>))
import System.IO (IO)
import Text.Show (show)

import Control.Monad.Freer (runM)
import Control.Monad.Freer.Fresh (evalFresh, fresh)
import Control.Monad.Freer.Trace (runTraceIO, trace)


-- | Generate two fresh values.
--
-- >>> traceFresh
-- Fresh 0
-- Fresh 1
traceFresh :: IO ()
traceFresh = runM . runTraceIO $ flip evalFresh 0 $ do
    n <- fresh
    trace $ "Fresh " <> show n
    n' <- fresh
    trace $ "Fresh " <> show n'

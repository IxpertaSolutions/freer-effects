{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}

module Control.Monad.Freer.Functor where

import Control.Applicative (pure)
import Control.Monad.Freer.Internal (Eff, replaceRelay)
import Prelude (flip, ($))


-- | A class witnessing that 'eff' forms a covariant functor in its second type
-- parameter.
class CoEff eff where
  effmap :: (a -> b) -> Eff (eff a ': effs) x -> Eff (eff b ': effs) x


-- | A class witnessing that 'eff' forms a contravariant functor in its second
-- type parameter.
class ContraEff eff where
  contraeffmap :: (b -> a) -> Eff (eff a ': effs) x -> Eff (eff b ': effs) x


-- | A class witnessing that 'eff' forms a invariant functor in its second
-- type parameter.
class InvEff eff where
  inveffmap :: (a -> b) -> (b -> a) -> Eff (eff a ': effs) x -> Eff (eff b ': effs) x


-- | Helper function for defining 'effmap', 'contraeffmap', and 'inveffmap' by
-- wrangling 'replaceRelay's type into something that can be type-inferred and
-- lambda-cased.
eff :: (forall v. (v -> Eff (eff b ': effs) x) -> eff a v -> Eff (eff b ': effs) x)
    -> Eff (eff a ': effs) x
    -> Eff (eff b ': effs) x
eff f = replaceRelay pure $ flip f


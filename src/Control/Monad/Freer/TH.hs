{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- | A collection of TemplateHaskell functions capable of generating the boring
-- @MonadX@ instances for transformer stacks that contain an @X@ somewhere
-- inside.
--
-- These instances can be automatically generated for any class without
-- additional superclass constraints.
module Control.Monad.Freer.TH
  ( liftInst
  ) where

import Language.Haskell.TH
import Control.Monad                (forM, join)
import Debug.Trace
import Data.Char (toLower)
import Control.Monad.Freer (send, Member, Eff)
import Data.Maybe (mapMaybe)

overFirst :: (a -> a) -> [a] -> [a]
overFirst f (a:as) = f a : as
overFirst _ as     = as


liftInst :: Name -> Q [Dec]
liftInst cls = do
  reify cls >>= \case
    TyConI dec -> buildInstance cls dec
    x -> error $ show x

buildInstance :: Name -> Dec -> Q [Dec]
buildInstance t (DataD _ name ts _ cons _) = do
  -- (h, clsTypeVars, dTypeVars) <- instanceHead name t
  -- let m = last dTypeVars
  dec <- mapM liftedFuncDecl cons
  sig <- mapM (generateSig t ts) cons
  return $ join dec ++ join sig

  -- return $ InstanceD Nothing [ AppT (ConT ''Monad) $ VarT m
  --                            , AppT (foldTypeApp name clsTypeVars) $ VarT m
  --                            ] h dec
buildInstance _ x = error $ "tried to build an instance for something that wasn't a class" ++ show x


------------------------------------------------------------------------------
-- | Builds a function definition of the form
--
-- > x a b c = lift $ x a b c
liftedFuncDecl :: Con -> Q [Dec]
liftedFuncDecl (ForallC _ _ con) = liftedFuncDecl con
liftedFuncDecl (GadtC [name] ts _) = do
  let name' = mkName . overFirst toLower $ nameBase name
  let arity = length ts - 1
  dTypeVars <- forM [0..arity] $ const $ newName "a"
  return [ FunD name'
           . pure
           $ Clause (VarP <$> dTypeVars)
                    (NormalB . AppE (VarE 'send) $ foldl (\b -> AppE b . VarE) (ConE name) dTypeVars)
                    []
         ]
liftedFuncDecl x = error $ "class contains something that is not a signature" ++ show x


generateSig :: Name -> [TyVarBndr] -> Con -> Q [Dec]
generateSig eff tvs (ForallC _ _ con) = generateSig eff tvs con
generateSig eff tvs (GadtC [name] ts c) = do
  let tvs' = init $ fmap unkinded tvs
  r <- newName "r"
  let name' = mkName . overFirst toLower $ nameBase name
  let ts' = fmap snd ts
      c' = unapp c
      tt = arrows $ ts' ++ [ConT ''Eff `AppT` VarT r `AppT` last c']
  return . pure
         . SigD name'
         . ForallT (PlainTV r : (fmap PlainTV $ mapMaybe isFree ts'))
                   [AppT (AppT (ConT ''Member) (head c')) $ VarT r]
         $ tt

  -- HowHow :: Int -> Bool -> s -> MyEffect s s

isFree :: Type -> Maybe Name
isFree (VarT n) = Just n
isFree _ = Nothing

unkinded :: TyVarBndr -> Name
unkinded (PlainTV n) = n
unkinded _ = error "bad!"

unapp :: Type -> [Type]
unapp (AppT a b) = a : unapp b
unapp x = [x]


arrows :: [Type] -> Type
arrows = foldr1 (AppT . AppT ArrowT)



------------------------------------------------------------------------------
-- | Given a 'Name' and a list of 'Name's, treads the first as a constructor,
-- and applies the remainder as type variables to it. Left associative.
foldTypeApp :: Name -> [Type] -> Type
foldTypeApp n = foldl (\b -> AppT b) (ConT n)


------------------------------------------------------------------------------
-- | Computes the arity of a function type.
-- Retrieved from http://www.parsonsmatt.org/2015/11/15/template_haskell.html
tyArity :: Type -> Int
tyArity = go 0
  where
    go :: Int -> Type -> Int
    go n (AppT (AppT ArrowT _) rest) =
      go (n+1) rest
    go n (ForallT _ _ rest) =
      go n rest
    go n _ =
      n

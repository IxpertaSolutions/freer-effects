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

overFirst :: (a -> a) -> [a] -> [a]
overFirst f (a:as) = f a : as
overFirst _ as     = as


liftInst :: Name -> Q [Dec]
liftInst cls = do
  reify cls >>= \case
    TyConI dec -> buildInstance cls dec
    x -> error $ show x

buildInstance :: Name -> Dec -> Q [Dec]
buildInstance t (DataD _ name _ _ cons _) = do
  -- (h, clsTypeVars, dTypeVars) <- instanceHead name t
  -- let m = last dTypeVars
  dec <- mapM (liftedFuncDecl t) cons
  return $ join dec
  -- return $ InstanceD Nothing [ AppT (ConT ''Monad) $ VarT m
  --                            , AppT (foldTypeApp name clsTypeVars) $ VarT m
  --                            ] h dec
buildInstance _ x = error $ "tried to build an instance for something that wasn't a class" ++ show x


------------------------------------------------------------------------------
-- | Builds a function definition of the form
--
-- > x a b c = lift $ x a b c
liftedFuncDecl :: Name -> Con -> Q [Dec]
liftedFuncDecl t (ForallC _ _ con) = liftedFuncDecl t con
liftedFuncDecl t (GadtC [name] ts _) = do
  let name' = mkName . overFirst toLower $ nameBase name
  let arity = length ts - 1
  dTypeVars <- forM [0..arity] $ const $ newName "a"
  r <- newName "r"
  return [ FunD name'
           . pure
           $ Clause (VarP <$> dTypeVars)
                    (NormalB . AppE (VarE 'send) $ foldl (\b -> AppE b . VarE) (ConE name) dTypeVars)
                    []
         , SigD name' $ ForallT [PlainTV r] [AppT (AppT (ConT ''Member) (ConT t)) $ VarT r] ((ConT ''Eff) `AppT` VarT r `AppT` (ConT $ tupleTypeName 0))
         ]
liftedFuncDecl _ x = error $ "class contains something that is not a signature" ++ show x


------------------------------------------------------------------------------
-- | Returns all the type variables in a 'Name's declaration.
getTyVars :: Name -> Q [Name]
getTyVars name =
    reify name >>= return . \case
      ClassI dec _ -> get dec
      TyConI dec   -> get dec
      _            -> []
  where
    get (DataD    _ _ xs _ _ _) = rip <$> xs
    get (NewtypeD _ _ xs _ _ _) = rip <$> xs
    get (ClassD _ _ xs _ _)     = rip <$> xs
    get _                       = []

    rip (PlainTV n)    = n
    rip (KindedTV n _) = n


------------------------------------------------------------------------------
-- | Builds an "instance head". Eg:
--
-- > instanceHead ''Monad ''ReaderT
--
-- builds
--
-- > (Monad (ReaderT s m)) =>
instanceHead :: Name -> Name -> Q (Type, [Name], [Name])
instanceHead cls d = do
  clsTypeVars <- init <$> getTyVars cls
  dTypeVars   <- init <$> getTyVars d
  return $ ( AppT (foldTypeApp cls clsTypeVars) $ foldTypeApp d dTypeVars
           , clsTypeVars
           , dTypeVars
           )


------------------------------------------------------------------------------
-- | Given a 'Name' and a list of 'Name's, treads the first as a constructor,
-- and applies the remainder as type variables to it. Left associative.
foldTypeApp :: Name -> [Name] -> Type
foldTypeApp n = foldl (\b -> AppT b . VarT) (ConT n)


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

{-# LANGUAGE TemplateHaskell, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module Expr where

import Data.Comp
import Data.Comp.Derive
import Data.Comp.Show ()
import Data.Comp.Equality ()
import Same

data Value a = I Int | B Bool
  deriving Functor

data Arith a = Add a a | Mult a a
  deriving Functor

data Logic a = Not a | And a a | Or a a | Eq a a
  deriving Functor

data Cond a = If a a a
  deriving Functor

type Sig = Value :+: Arith :+: Logic :+: Cond

$(derive [makeTraversable, makeFoldable,
          makeEqF, makeShowF, smartConstructors, smartAConstructors]
         [''Value, ''Arith, ''Logic, ''Cond])

instance SameF Value where
  sameF _ _ = False

instance SameF Arith where
  sameF (Add _ _) (Add _ _) = True
  sameF (Mult _ _) (Mult _ _) = True
  sameF _ _ = False

instance SameF Logic where
  sameF (Not _) (Not _) = True
  sameF (And _ _) (And _ _) = True
  sameF (Or _ _) (Or _ _) = True
  sameF (Eq _ _) (Eq _ _) = True
  sameF _ _ = False

instance SameF Cond where
  sameF _ _ = True

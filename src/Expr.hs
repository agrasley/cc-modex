{-# LANGUAGE TemplateHaskell, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Expr where

import Data.Comp
import Data.Comp.Derive
import Data.Comp.Show ()
import Data.Comp.Equality ()
import Choices
import Factor

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

instance (Arith :<: g, Choice :<: g) => Factor Arith g where
  factorC' t (Add l r) (Add l' r') = Just (iChc t l l' `iAdd` iChc t r r')
  factorC' t (Mult l r) (Mult l' r') = Just (iChc t l l' `iAdd` iChc t r r')
  factorC' _ _ _ = Nothing

instance (Value :<: g, Choice :<: g) => Factor Value g where
  factorC' _ _ _ = Nothing

instance (Logic :<: g, Choice :<: g) => Factor Logic g where
  factorC' t (Not x) (Not y) = Just (iNot (iChc t x y))
  factorC' t (And l r) (And l' r') = Just (iChc t l l' `iAnd` iChc t r r')
  factorC' t (Or l r) (Or l' r') = Just (iChc t l l' `iOr` iChc t r r')
  factorC' t (Eq l r) (Eq l' r') = Just (iChc t l l' `iEq` iChc t r r')
  factorC' _ _ _ = Nothing

instance (Cond :<: g, Choice :<: g) => Factor Cond g where
  factorC' t (If c t' e) (If c' t'' e') = Just (iIf (iChc t c c') (iChc t t' t'') (iChc t e e'))

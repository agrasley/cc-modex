{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Factor where

import Data.Comp
import Data.Comp.Ops
import Choices
import Expr
import Generic


class (f :<: g, Choice :<: g) => Factor f g where
  factorC' :: Tag -> f (Term g) -> f (Term g) -> Maybe (Term g)

instance ((f :+: g) :<: h, Choice :<: h, Factor f h, Factor g h) => Factor (f :+: g) h where
  factorC' t (Inl x) (Inl y) = factorC' t x y
  factorC' t (Inr x) (Inr y) = factorC' t x y
  factorC' _ _ _ = Nothing

instance (Choice :<: g) => Factor Choice g where
  factorC' _ _ _ = Nothing

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

factorC :: (Factor dom dom) => Tag -> Term dom -> Term dom -> Maybe (Term dom)
factorC t l r = factorC' t (unTerm l) (unTerm r)

factor' :: (Choice :<: dom, Factor dom dom) => Term dom -> Term dom
factor' v | Just (Chc t a b) <- project v,
            Just c <- factorC t a b        = c
          | otherwise                      = v

factor :: (Choice :<: dom, Factor dom dom, Functor dom) => Term dom -> Term dom
factor = descend factor'

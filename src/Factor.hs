{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Factor where

import Data.Comp
import Data.Comp.Ops
import Choices
import Generic


class (f :<: g, Choice :<: g) => Factor f g where
  factorC' :: Tag -> f (Term g) -> f (Term g) -> Maybe (Term g)

instance ((f :+: g) :<: h, Choice :<: h, Factor f h, Factor g h) =>
         Factor (f :+: g) h where
  factorC' t (Inl x) (Inl y) = factorC' t x y
  factorC' t (Inr x) (Inr y) = factorC' t x y
  factorC' _ _ _ = Nothing

instance (Choice :<: g) => Factor Choice g where
  factorC' _ _ _ = Nothing

factorC :: (Factor dom dom) => Tag -> Term dom -> Term dom -> Maybe (Term dom)
factorC t l r = factorC' t (unTerm l) (unTerm r)

factor' :: (Choice :<: dom, Factor dom dom) => Term dom -> Term dom
factor' v | Just (Chc t a b) <- project v,
            Just c <- factorC t a b        = c
          | otherwise                      = v

factor :: (Choice :<: dom, Factor dom dom, Functor dom) => Term dom -> Term dom
factor = descend factor'

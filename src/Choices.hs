{-# LANGUAGE TemplateHaskell, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module Choices where

import Data.Comp
import Data.Comp.Derive
import Data.Comp.Show ()
import Data.Comp.Equality ()
import Same
import Generic

type Tag = String

data Alt = L | R

type Selection = [(Tag,Alt)]

data Choice a = Chc Tag a a
  deriving Functor

$(derive [makeTraversable, makeFoldable,
          makeEqF, makeShowF, smartConstructors, smartAConstructors]
         [''Choice])

instance SameF Choice where
  sameF _ _ = False

idemp' :: (Choice :<: dom, EqF dom) => Term dom -> Term dom
idemp' v | Just (Chc _ a b) <- project v, a == b = a
         | otherwise = v

idemp :: (Choice :<: dom, EqF dom, Functor dom) => Term dom -> Term dom
idemp = transform idemp'

select' :: (Choice :<: dom) => Selection -> Term dom -> Term dom
select' s v | Just (Chc t a b) <- project v,
              Just L <- lookup t s           = a
            | Just (Chc t a b) <- project v,
              Just R <- lookup t s           = b
            | otherwise                      = v

select :: (Choice :<: dom, Functor dom) => Selection -> Term dom -> Term dom
select s = descend' (select' s)

dominate' :: (Choice :<: dom, Functor dom) => Term dom -> Term dom
dominate' v | Just (Chc t a b) <- project v = iChc t (select [(t,L)] a) (select [(t,R)] b)
            | otherwise                     = v

dominate :: (Choice :<: dom, Functor dom) => Term dom -> Term dom
dominate = descend dominate'

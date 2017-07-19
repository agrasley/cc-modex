{-# LANGUAGE TypeOperators,
             MultiParamTypeClasses,
             FlexibleInstances,
             FlexibleContexts,
             UndecidableInstances #-}

module Eval where

import Data.Comp
import Data.Comp.Show ()
import Data.Comp.Derive
import Data.Comp.Equality
import Data.Comp.Ops
import Expr

-- Term evaluation algebra
class Eval f v where
  evalAlg :: Alg f (Term v)

instance {-# OVERLAPPING #-} (Eval f v, Eval g v) => Eval (f :+: g) v where
  evalAlg (Inl x) = evalAlg x
  evalAlg (Inr x) = evalAlg x

projI :: (Value :<: dom) => Term dom -> Int
projI v = case project v of Just (I i) -> i

projB :: (Value :<: dom) => Term dom -> Bool
projB v = case project v of Just (B b) -> b

-- Lift the evaluation algebra to a catamorphism
eval :: (Functor f, Eval f v) => Term f -> Term v
eval = cata evalAlg

instance {-# OVERLAPPING #-} (f :<: v) => Eval f v where
  evalAlg = inject -- default instance

instance {-# OVERLAPPING #-} (Value :<: dom) => Eval Arith dom where
  evalAlg (Add x y)  = iI $ projI x + projI y
  evalAlg (Mult x y) = iI $ projI x * projI y

instance {-# OVERLAPPING #-} (Value :<: dom, EqF dom) => Eval Logic dom where
  evalAlg (Eq x y) = iB $ x == y
  evalAlg (And x y) = iB $ projB x && projB y
  evalAlg (Or x y) = iB $ projB x || projB y
  evalAlg (Not x) = iB $ (not . projB) x

instance {-# OVERLAPPING #-} (Value :<: dom) => Eval Cond dom where
  evalAlg (If x y z) = if projB x then y else z

evalEx :: Term Value
evalEx = eval (iIf (iNot (iI 3 `iEq` (iI 1 `iAdd` iI 2))) (iB True `iAnd` iB False) (iI 4 `iMult` iI 5) :: Term Sig)

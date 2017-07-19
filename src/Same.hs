{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Same where

import Data.Comp
import Data.Comp.Ops


class Same a where
  same :: a -> a -> Bool

instance Same () where
  same _ _ = True

class SameF f where
  sameF :: Same a => f a -> f a -> Bool

instance (SameF f, Same a) => Same (Cxt h f a) where
  same = sameF

instance (SameF f) => SameF (Cxt h f) where
  sameF (Term e1) (Term e2) = e1 `sameF` e2
  sameF _ _ = False

instance (SameF f, SameF g) => SameF (f :+: g) where
  sameF (Inl x) (Inl y) = sameF x y
  sameF (Inr x) (Inr y) = sameF x y
  sameF _ _ = False

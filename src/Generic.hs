module Generic where

import Data.Comp.Term

{-
transform :: (Functor f) => (Term f -> Term f) -> Term f -> Term f
transform f = run
    where run = f . Term . fmap run . unTerm
-}

descend :: (Functor f) => (Term f -> Term f) -> Term f -> Term f
descend f = run
  where run = Term . fmap run . unTerm . f

descend' :: (Functor f) => (Term f -> Term f) -> Term f -> Term f
descend' f = run
  where run = f . Term . fmap run . unTerm . f

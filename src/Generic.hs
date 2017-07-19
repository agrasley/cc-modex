module Generic where

import Data.Comp.Term

{-
transform :: (Functor f) => (Term f -> Term f) -> Term f -> Term f
transform f = run
    where run = f . Term . fmap run . unTerm
-}

{-
transform :: Uniplate on => (on -> on) -> on -> on
transform f x = f $ generate $ map (transform f) current
    where (current, generate) = uniplate x

descend :: Uniplate on => (on -> on) -> on -> on
descend f x = generate $ map f current
    where (current, generate) = uniplate x
-}

descend :: (Functor f) => (Term f -> Term f) -> Term f -> Term f
descend f = run
  where run = Term . fmap run . unTerm . f

descend' :: (Functor f) => (Term f -> Term f) -> Term f -> Term f
descend' f = run
  where run = f . Term . fmap run . unTerm . f

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE RankNTypes            #-}

module Configure where

import Data.Comp
import Choices

class (Functor f, Functor g) => Configure f g where
    configHom :: [Tag] -> Hom f g
    configHom xs = configHom' xs . fmap Hole
    configHom' :: [Tag] -> Alg f (Context g a)
    configHom' xs x = appCxt (configHom xs x)

instance {-# OVERLAPPING #-} (Configure f h, Configure g h) =>
         Configure (f :+: g) h where
    configHom xs = caseF (configHom xs) (configHom xs)

configure :: Configure f g => [Tag] -> Term f -> Term g
configure xs = appHom (configHom xs)

configureA :: (Functor f', Functor g', DistAnn f p f', DistAnn g p g',
              Configure f g) => [Tag] -> Term f' -> Term g'
configureA xs = appHom (propAnn (configHom xs))

instance {-# OVERLAPPING #-} (Functor f, Functor g, f :<: g) =>
         Configure f g where
    configHom _ = simpCxt . inj

instance {-# OVERLAPPING #-} (Functor f) => Configure Choice f where
  configHom' ts (Chc t a b) | t `elem` ts = a
                            | otherwise   = b

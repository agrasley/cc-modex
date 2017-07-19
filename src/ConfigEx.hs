{-# LANGUAGE TypeOperators #-}

module ConfigEx where

import Data.Comp
import Choices
import Configure
import Eval
import Expr
import Factor

type Sig' = Choice :+: Value :+: Arith :+: Logic :+: Cond

evalConfig :: [Tag] -> Term Sig' -> Term Value
evalConfig ts = eval . (configure ts :: Term Sig' -> Term Sig)

configEx :: Term Value
configEx = evalConfig ["B"] (iChc "A" (iI 1) (iChc "B" (iI 2 `iAdd` iI 4) (iI 3)) :: Term Sig')

idempEx :: Term Sig'
idempEx = idemp (iChc "B" (iChc "A" (iI 1) (iI 1)) (iI 1))

selectEx :: Term Sig'
selectEx = select [("A",R),("C",R)] (iChc "C" (iI 0) (iChc "A" (iI 1) (iI 2)) `iAdd` iChc "B" (iI 3) (iI 4))

dominateEx :: Term Sig'
dominateEx = dominate (iChc "A" (iChc "A" (iI 1) (iI 2)) (iChc "A" (iI 3) (iI 4) `iAdd` iI 5))

factorEx :: Term Sig'
factorEx = factor (iChc "A" (iI 1 `iAdd` iI 2) (iI 3 `iAdd` iI 4))

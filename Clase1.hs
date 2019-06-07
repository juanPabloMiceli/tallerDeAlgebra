module Clase1
where

esPar :: Integer -> Bool
esPar x = mod x 2 == 0

esMultiploDe ::  Integer -> Integer -> Bool

esMultiploDe x y | mod y x == 0 = True
                 | otherwise = False


impar :: Integer -> Bool

impar x = mod x 2 /= 0



identidad :: b -> b
identidad x = x



crearPar :: a -> b -> (a,b)
crearPar x y = (x,y)

invertir :: (a,b) -> (b,a) 
invertir p = (snd(p),fst(p))

distR1 :: (Float, Float) -> Float 
distR1 (x, y) = sqrt(x*x+y*y)


distR2 :: (Float, Float) -> (Float, Float) -> Float 
distR2 (x, y) (z, w) = distR1(( z-x, w-y))

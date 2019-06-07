module Clase
where 
import Clase1

type Set a = [a]

unidades :: Integer -> Integer

unidades x = mod x 10
 

sumaUnidades3 :: Integer -> Integer -> Integer -> Integer

sumaUnidades3 x y z =  unidades x + unidades y + unidades z 


todosImpares :: Integer -> Integer -> Integer -> Bool

todosImpares x y z | impar x && impar y && impar z = True
                   | otherwise = False

alMenos1Impar :: Integer -> Integer -> Integer -> Bool

alMenos1Impar x y z | impar x || impar y || impar z = True
                    | otherwise = False

alMenos2Impares :: Integer -> Integer -> Integer -> Bool

alMenos2Impares x y z | impar x && impar y && impar z = True
                      | impar x && impar y = True
                      | impar x && impar z = True
                      | impar y && impar z = True
                      | otherwise = False


alMenos2Pares :: Integer -> Integer -> Integer -> Bool

alMenos2Pares x y z | alMenos2Impares x y z = False
                    | otherwise = True

r1 :: Integer -> Integer -> Bool

r1 a b = esPar (a+b)
       
r2 :: Integer -> Integer -> Bool

r2 a b = esMultiploDe 5 (2*a+3*b)
       

r3 :: Integer -> Integer -> Bool

r3 a b | unidades a == unidades b  || unidades a == unidades (a*b) || unidades b == unidades (a*b)  = False
       | otherwise = True


r4 :: Integer -> Integer -> Bool

r4 x y | x < 3 && y < 3 = True
       | x >= 3 && y >= 3 = True
       | otherwise = False 

r5 :: Integer -> Integer -> Bool

r5 x y | x < 3 && y < 3 = True
       | x >= 3 && x < 7 && y >= 3 && y < 7 = True
       | x >= 7 && y >= 7 = True
       | otherwise = False 


r6 :: (Integer, Integer) -> (Integer, Integer) -> Bool

r6 p q | fst(p) == 0 && snd(p) == 0 && fst(q) == 0 && snd(q) == 0 = False


existeMultiplo :: (Integer, Integer) -> (Integer, Integer) -> Bool
existeMultiplo p q | esMultiploDe (fst p) (fst q) = True
                   | otherwise = False


factorial :: Integer -> Integer
factorial n | n == 0 = 1
            | otherwise = n * factorial (n-1)


fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n > 1  = fibonacci(n - 1) + fibonacci(n - 2) 

serie1 :: Integer -> Integer
serie1 n    | n == 1 = 2
            | n > 1  = (2 * (n-1) * serie1(n-1)) + ((2 ^ n) * factorial(n-1))
            | otherwise = -1 


serie2 :: Integer -> Integer
serie2 n    | n == 1 = n
            | n == 2 = n
            | n > 2  = (n-2)* serie2(n-1) + 2*(n-1) * serie2(n-2)

serie3 :: Integer -> Integer
serie3 n    | n == 1 = -3
            | n == 2 = 6
            | esPar n == False && n > 2  = -(serie3 (n-1)) - 3
            | n > 2  = serie3 (n-1) + 2 * serie3 (n-2) + 9 

sumatoria1 :: Integer -> Integer
sumatoria1 n | n == 0 = 1
             | n > 0 = 2 ^ n + sumatoria1(n-1)


sumatoria2 :: Integer -> Float -> Float
sumatoria2 n q | n == 1 = q 
               | n > 1 = (q ^ n) + sumatoria2 (n-1) q


sumatoria3 :: Integer -> Float -> Float
sumatoria3 n q | n == 0 = 1
               | n >  0 = (q ^ (2*n)) + q ^ (2*n-1) + sumatoria3 (n-1) q

sumatoria4 :: Integer -> Float -> Float
sumatoria4 n q | n == 0 = 1
               | n >  0 = (q ^ (2*n)) + q ^ (2*n-1) + sumatoria4 (n-1) q - q ^ (n-1)

eAprox :: Integer -> Double
eAprox n | n == 0 = 1
         | n > 0 = 1/fromInteger (factorial n) + eAprox (n-1)

e :: Double
e = eAprox 1000

parteEntera :: Float -> Integer
parteEntera x | x < 1 && x >= 0 = 0
              | x >= 1 = parteEntera (x-1) + 1
              | x > -1 && x < 0 = -1
              | x <= (-1) = parteEntera (x+1) - 1

division :: Integer -> Integer -> (Integer, Integer)
division a d | a >= 0 && a < d = (0, a)
             | a <= 0 && abs a < d = (-1, a+d)
             | a > 0 = (fst qr + 1, snd qr)
             | a < 0 = (fst qr2 -1, abs (snd qr2)) 
              where qr = division (a-d) d
                    qr2 = division (a+d) d 

sumaDivisores :: Integer -> Integer
sumaDivisores n = sumaDivisoresHasta n n

sumaDivisoresHasta :: Integer -> Integer -> Integer
sumaDivisoresHasta n k | k == 1 = 1
                       | k > 1 && (snd (division n k ) == 0) = k +sumaDivisoresHasta n (k-1)   
                       | k > 1 && (snd (division n k ) /= 0) = sumaDivisoresHasta n (k-1)              

sumaImpares :: Integer -> Integer
sumaImpares n | n == 1 = n
              | n > 1  = (2*n-1) + sumaImpares(n-1)
              
multiplo3 :: Integer -> Bool
multiplo3 n | n == 0 = True
            | n == 1 = False
            | n == 2 = False
            | n == 3 = True
            | n > 3 = multiplo3 (n-3)

medioFact :: Integer -> Integer
medioFact n | n == 1 = 1
            | n == 2 = 2
            | n > 2 = n * medioFact (n-2)


esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo n = menorDivisor n 2 == n

menorDivisor :: Integer -> Integer ->Integer
menorDivisor n k | mod n k == 0 = k
                 | otherwise =  menorDivisor n (k+1)


sumaExterna :: Integer -> Integer -> Integer
sumaExterna n m | n == 1 = sumaInterna 1 m
                | n > 1 = sumaInterna n m + sumaExterna (n-1) m

sumaInterna :: Integer -> Integer -> Integer
sumaInterna i k | k == 1 = i
                | k > 1 = sumaInterna i (k-1) + i^k
                
                
yLogico :: Bool -> Bool ->  Bool
yLogico True True = True
yLogico _ _ = False


oLogico :: Bool -> Bool -> Bool
oLogico False False = False
oLogico _ _ = True


implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True


sumaGaussiana :: Integer -> Integer
sumaGaussiana 0 = 0
sumaGaussiana n = sumaGaussiana (n-1) + n


algunCero :: (Integer, Integer, Integer) -> Bool
algunCero (0,_,_) = True
algunCero (_,0,_) = True
algunCero (_,_,0) = True
algunCero _ = False

algunCero2 :: (Integer, Integer, Integer) -> Bool
algunCero2 (x1,x2,x3) = x1*x2*x3 == 0 

productoInterno :: (Float, Float) -> (Float, Float) -> Float
productoInterno (x1, y1) (x2, y2) = x1*x2 + y1*y2


sumaDeDigitos :: Integer -> Integer
sumaDeDigitos x | x < 10 = x
                | otherwise = sumaDeDigitos (div x 10)  + mod x 10 

digitosIguales :: Integer -> Bool
digitosIguales x | x < 10 = True
                 | x >= 10 = (mod x 10) == mod (div x 10) 10 && digitosIguales (div x 10)



sumaDePrimos :: Integer -> Bool
sumaDePrimos n = funcionAux1 n 1

funcionAux1 :: Integer -> Integer -> Bool
funcionAux1 n k | k <= (div n 2) = ( esPrimo k && esPrimo (n-k) ) || ( funcionAux1 n (k+1) )
                | k > (div n 2) = False 

{-
sucesionCollatz :: Integer -> Integer
sucesionCollatz n = comparoDeA2 1 n

comparoDeA2 :: Integer -> Integer -> Integer
comparoDeA2 k n | k < n = max (collatz k 1) (collatz k+1 1)
                | k == n | 
                
collatz :: Integer -> Integer -> Integer
collatz k n | k != 1 =  

| n == 1 = 0
                  | n == 2 = 1
                  | mod n 2 == 0 = n/2
                  | otherwise = 3n + 1



numeroCollatz :: Integer -> Integer 
numeroCollatz n = algo (n 1)

algo Integer -> Integer -> (Integer,Integer)
algo n k | n \= 1 = (algo (collatz n),k+1)
         | otherwise = 




collatz n | n == 1 = 0
          | mod n 2 == 0 = n/2
          | otherwise = 3*n+1


-}

primero = head [(1,2),(3,4),(5,6),(7,8)]
loDemas = tail [(1,2),(3,4),(5,6),(7,8)]

concatenar = [True, True] : []
concatenar2 = [1,2] : [] 


sumatoriaLista :: [Integer] -> Integer
sumatoriaLista n | length n /= 1 = head n + sumatoriaLista (tail n)
                 | otherwise = head n 

listar :: Integer -> Integer -> Integer ->  [Integer] 
listar a b c = a : b : c : [] 

pertenece :: Integer -> [Integer] -> Bool
pertenece n l | length l /= 1 = head l == n || pertenece n (tail l) 
              | otherwise = False

expresion = [1,0..(-100)]

productoriaListas :: [Integer] -> Integer
productoriaListas l | length l /= 1 = productoriaListas (tail l) * head l
                    | otherwise = head l

sumaLista :: Integer -> [Integer] -> [Integer]
sumaLista n l | length l /= 1 = (head l + n) : sumaLista n (tail l)
              | otherwise = [head l + n]

sumaElPrimero :: [Integer] -> [Integer]
sumaElPrimero l = sumaLista (head l) l

sumaElUltimo :: [Integer] -> [Integer]
sumaElUltimo l = sumaLista (lastDigit l) l

lastDigit :: [Integer] -> Integer
lastDigit l | length l /= 1 = lastDigit (tail l)
            | otherwise = head l

pertenecePM :: Integer -> [Integer] -> Bool
pertenecePM _ [] = False
pertenecePM n (x:xs) =  n == x || pertenecePM n xs 

multiplos :: Integer -> [Integer] -> [Integer]
multiplos _ [] = []
multiplos n (h:t) | mod h n == 0 = h : multiplos n t 
                  | otherwise = multiplos n t

quitar :: Integer -> [Integer] -> [Integer]
quitar _ [] = []
quitar n (h:t) | n == h = t
               | otherwise = h : (quitar n t)

hayRepetidos :: [Integer] -> Bool
hayRepetidos [] = False
hayRepetidos (h:t) = pertenecePM h t || hayRepetidos t 

eliminarRepetidos :: [Integer] -> [Integer]
eliminarRepetidos [] = []
eliminarRepetidos (h:t) | pertenecePM h t = eliminarRepetidos t
                        | otherwise = h : eliminarRepetidos t

longitud :: [Integer] -> Integer
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

imparesHasta :: Integer -> [Integer]
imparesHasta 0 = []
imparesHasta t | mod t 2 == 1 = t : imparesHasta (t-1)
               | otherwise = imparesHasta (t-1)

multiplosDe7 :: [Integer] -> [Integer]
multiplosDe7 [] = []
multiplosDe7 (x:xs) | mod x 7 == 0 = x : multiplosDe7 xs
                    | otherwise = multiplosDe7 xs  

agregar :: Integer -> Set Integer -> Set Integer
agregar n c | elem n c = c
            | otherwise = n : c

incluidoI :: Set Integer -> Set Integer -> Bool
incluidoI [] _ = True
incluidoI (x:xs) c = elem x c && incluidoI xs c 

                  
iguales :: Set Integer -> Set Integer -> Bool
iguales a b = incluidoI a b && incluidoI b a

mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (mod a b)


divisores :: Integer -> [Integer]
divisores n = divAux n 1

divAux :: Integer -> Integer -> [Integer]
divAux n k | k == (div n 2)+1 = [n]
           | mod n k == 0 = k : divAux n (k+1)
           | otherwise = divAux n (k+1)    

mcd2 :: Integer -> Integer -> Integer
mcd2 n1 n2 = mcd2Aux (reverse (divisores n1)) (reverse (divisores n2))

mcd2Aux::[Integer]->[Integer]->Integer
mcd2Aux (x:xs) (y:ys)| elem x (y:ys) = x
                     | otherwise= mcd2Aux xs (y:ys)  


fst3 (x, _, _) = x
snd3 (_, y, _) = y
trd3 (_, _, z) = z

emcd :: Integer -> Integer -> (Integer,Integer,Integer)
emcd 1 _ = (1,1,0)
emcd _ 1 = (1,0,1)
emcd 0 b = (b,0,1)
emcd a 0 = (a,1,0)
emcd a b = (mcd a b,trd3 (emcd b (mod a b)), snd3 (emcd b (mod a b)) - trd3 (emcd b (mod a b))*(div a b))

tieneSol :: Integer -> Integer -> Integer -> Bool
tieneSol a b m = mod b (mcd a m) == 0

solPart :: Integer -> Integer -> Integer -> Integer
solPart a b m | tieneSol a b m == True = (snd3 (emcd a m)) * (div b (mcd a m))
             
solGen::Integer -> Integer -> Integer -> (Integer,Integer)
solGen a b m=(solPart a b m, div m (mcd a m))


individuoPerteneceATupla :: Integer -> (Integer, Integer) -> Bool
individuoPerteneceATupla n (x1,x2) = n == x1 || n == x2

aislarAmigosDeUsuario :: Integer -> [(Integer,Integer)] -> [(Integer,Integer)]
aislarAmigosDeUsuario _ [] = []
aislarAmigosDeUsuario n (x:xs) | individuoPerteneceATupla n x = x : aislarAmigosDeUsuario n xs
                                  | otherwise = aislarAmigosDeUsuario n xs

{-
perteneceAConjunto :: Set Integer -> Set (Set Integer) -> Bool
perteneceAConjunto _ [] = False
perteneceAConjunto a [(x:xs)] = iguales a [x] || perteneceAConjunto a [xs]

productoCartesiano :: Set Integer -> Set Integer -> Set (Set Integer) 
productoCartesiano [] ys = []
productoCartesiano xs [] = []
productoCartesiano [x] ys = (x, head ys) : productoCartesiano [x] (tail ys)
productoCartesiano xs ys = productoCartesiano [head xs] ys ++ productoCartesiano (tail xs) ys

variaciones :: Set Integer -> Integer -> Set [Integer]
variaciones xs 0 = [[]]
variaciones xs k = (variaciones xs k-1)
-}

maximo :: [Integer] -> Integer
maximo [x] = x
maximo (x:xs) = max x (maximo xs)

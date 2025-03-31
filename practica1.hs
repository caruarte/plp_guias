
-- ej 1

max2 :: (Float, Float) -> Float
max2 (x, y) 
    | x >= y = x
    | otherwise = y

-- No Está curryficada

max2Curry :: Float -> Float -> Float
max2Curry x y
    | x >= y = x
    | otherwise = y

normaVectorial :: (Float, Float) -> Float
normaVectorial (x, y) = sqrt (x^2 + y^2)

-- No Está curryficada

normaVectorialCurry :: Float -> Float -> Float
normaVectorialCurry x y = sqrt (x^2 + y^2)

substract :: Float -> Float -> Float
substract = flip (-)

-- Está currificada

predecesor :: Float -> Float
predecesor = substract 1

-- Está currificada

evaluarEnCero :: (Float -> Float) -> Float
evaluarEnCero = \f -> f 0

-- Está currificada

dosVeces :: (Float -> Float) -> (Float -> Float)
dosVeces = \f -> f . f

-- Está currificada

flipAll :: [Float -> Float -> Float] -> [Float -> Float -> Float]
flipAll = Prelude.map flip

-- Está currificada

-- flipRaro :: (Float -> Float -> Float) -> Float -> Float -> Float 
-- flipRaro = flip flip

-- NOSE

-- ej 2

curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

-- curryN
-- NOSE

-- ej 3

sum :: (Num a) => [a] -> a
sum xs = foldr (+) 0 xs

elem :: (Eq a) => a -> [a] -> Bool
elem a xs = foldr (\x y -> x == a || y) False xs 

-- en foldr el caso base va a estar en y, a la derecha
-- en foldl va a estar en x, a la izquierda

(++) :: [a] -> [a] -> [a]
(++) xs ys = foldr (\x y -> x : y) ys xs

filter :: (a -> Bool) -> [a] -> [a]
filter f xs = foldr (\x y -> if f x then x : y else y) [] xs

map :: (a -> b) -> [a] -> [b]
map f xs = foldr (\x y -> f x : y) [] xs

mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f xs = foldr1 (\x y -> if (f x y) then x else y) xs

sumasParciales :: Num a => [a] -> [a]
sumasParciales xs = foldr (\x y -> x : (Prelude.map (\z -> z + x) y)) [] xs

sumasAlt :: Num a => [a]  -> a
sumasAlt xs = foldr (-) 0 xs

sumasAltInverso :: Num a => [a]  -> a
sumasAltInverso xs = sumasAlt (reverse xs) -- se puede hacer con reverse?

-- ej 4

permutaciones :: [a] -> [[a]]
permutaciones xs = foldr (\x rec -> concatMap (\xs -> Prelude.map (f x xs) [0..length xs]) rec) [[]] xs
                where f x xs index = drop index xs Prelude.++ [x] Prelude.++ take index xs

-- ej 5

-- elementosEnPosicionesPares usa recursión estructural???

-- entrelazar usa recursión estructural


entrelazar :: [a] -> [a] -> [a] -- PREGUNTAR
entrelazar xs ys = foldr (\x rec ys -> if null ys
    then x : rec []
    else x : head ys : rec (tail ys)) id xs ys


-- ej 6

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x : xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e xs = recr (\x xs rec -> if x == e then xs else x : rec) [] xs

-- por cada llamado recursivo hay un x y un xs distintos.

-- b) foldr no es adecuado ya que no guarda en su recursión al xs en cada momento
-- QUiero que se "resetee" la lista en algun momento

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado e xs = recr (\x xs rec -> if e < x then e : x : xs else x : rec) [e] xs

-- ej 7

mapPares :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares f xs = foldr (\x rec -> Main.uncurry f x : rec) [] xs

armarPares :: [a] -> [b] -> [(a, b)]
armarPares = foldr (\x rec ys -> 
    if null ys
        then []
        else (x, head ys) : rec (tail ys)) (const [])

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f = foldr (\x rec ys ->
    if null ys
        then []
        else f x (head ys) : rec (tail ys)) (const [])

-- foldr en armarPares y mapDoble es una funcion de [b] -> [c]
-- no como los normales que foldr devuelve [b]

-- ej  8

sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat xs ys = zipWith (\x y -> zipWith (+) x y) xs ys

-- trasponer :: [[Int]] -> [[Int]]
-- NOSE

-- ej 9

foldNat :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer
foldNat f x 0 = x
foldNat f x n = f n (foldNat f x (n-1))

potencia :: Integer -> Integer -> Integer
potencia b p = foldNat (\x rec -> b * rec) 1 p 

-- ej 10

-- genLista :: a -> (a-> a) -> Integer -> [a]
-- genLista _ _ 0 = []
-- genLista elem f len = elem : genLista (f elem) f (len - 1)

genLista :: a -> (a-> a) -> Integer -> [a]
genLista elem f len = foldr (\x rec ys -> ys : rec (f ys)) (const []) [1..len] elem

desdeHasta :: Integer -> Integer -> [Integer]
desdeHasta d h = genLista d (+1) (h - d + 1) 

-- ej 11

data Polinomio a = X | Cte a | Suma (Polinomio a) (Polinomio a) | Prod (Polinomio a) (Polinomio a)

foldPol :: Num a => Polinomio a -> a -> a
foldPol X elem = elem 
foldPol (Cte x) elem = x
foldPol (Suma x y) elem = foldPol x elem + foldPol y elem
foldPol (Prod x y) elem = foldPol x elem * foldPol y elem

evaluar :: Num a => a -> Polinomio a -> a
evaluar x pol = foldPol pol x

-- ej 12

data AB a = Nil | Bin (AB a) a (AB a) deriving Show

foldAB :: (a -> b -> b -> b) -> b -> AB a -> b
foldAB _ b Nil = b
foldAB f b (Bin izq r der) = f r (foldAB f b izq) (foldAB f b der)

recAB :: (a -> AB a -> AB a -> b -> b -> b) -> b -> AB a -> b
recAB _ b Nil = b
recAB f b (Bin izq r der) = f r izq der (recAB f b izq) (recAB f b der)

esNil :: AB a -> Bool
esNil Nil = True
esNil _ = False

altura :: AB a -> Integer
altura x = foldAB (\x izq der -> 1 + max izq der) 0 x

cantNodos :: AB a -> Integer
cantNodos x = foldAB (\x izq der -> 1 + izq + der) 0 x

-- mejorSegunAB :: (a -> a -> Bool) -> AB a -> a
-- mejorSegunAB f x = foldAB (\x izq der -> if f x izq then (if f x der then x else der) else (if f izq der then izq else der)) Nil x

-- NOSE COMO HACER LO DE NIL

esABB :: Ord a => AB a -> Bool
esABB arbol = recAB (\x (Bin _ izq _) (Bin _ der _) rec1 rec2 -> rec1 && rec2 && (if izq <= x then (if der > x then True else False) else False)) True arbol

-- NOSE COMO HACER CON EL CASO DE NIL
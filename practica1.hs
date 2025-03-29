
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


entrelazar :: [a] -> [a] -> [a]
entrelazar = foldr (\x rec ys -> if null ys
    then x : rec []
    else x : head ys : rec (tail ys)) id


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
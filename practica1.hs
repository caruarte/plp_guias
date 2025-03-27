
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
flipAll = map flip

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

(++) :: [a] -> [a] -> [a]
(++) xs ys = foldr (\x y -> x : y) ys xs

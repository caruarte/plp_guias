import System.Win32 (xBUTTON1, sCS_32BIT_BINARY)
import Distribution.Simple.Setup (TestShowDetails(Failures))
-- null :: [a] -> Bool
-- returns True if a list is empty, otherwise False

-- head :: 	[a] -> a
-- returns the first item of a list

-- tail :: [a] -> [a]
-- it accepts a list and returns the list without its first item

-- init :: [a] -> [a]
-- it accepts a list and returns the list without its last item

-- last :: 	[a] -> a
-- returns the last item of a list

-- take :: Int -> [a] -> [a]
-- creates a list, the first argument determines, how many items should be taken from the beginning of the list passed as the second argument

-- drop ::	Int -> [a] -> [a]
-- creates a list, the first argument determines, how many items should be taken from the end of the list passed as the second argument

-- 	(++) :: [a] -> [a] -> [a]
-- concatenates two lists

-- concat :: [[a]] -> [a]
-- accepts a list of lists and concatenates them

-- reverse :: [a] -> [a]
-- creates a new list from the original one with items in the reverse order

-- elem :: Eq a => a -> [a] -> Bool
-- returns True if the list contains an item equal to the first argument

-- ej 2

valorAbsoluto :: Float -> Float
valorAbsoluto x
    | x >= 0 = x
    | otherwise = -x

bisiesto :: Int -> Bool
bisiesto x
    | mod x 4 /= 0 = False
    | mod x 100 == 0 && mod x 400 /= 0 = False
    | otherwise = True

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x-1)

valorAbsolutoInt :: Int -> Int
valorAbsolutoInt x
    | x >= 0 = x
    | otherwise = -x

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos 0 = 0
cantDivisoresPrimos x = length (divisoresPrimos (divisoresPositivos (valorAbsolutoInt x) (valorAbsolutoInt x)))

divisoresPrimos :: [Int] -> [Int]
divisoresPrimos [] = []
divisoresPrimos (x:xs)
    | esPrimo x = x : divisoresPrimos xs
    | otherwise = divisoresPrimos xs

divisoresPositivos :: Int -> Int -> [Int]
divisoresPositivos x 0 = []
divisoresPositivos x y
    | mod x y == 0 = y : divisoresPositivos x (y-1)
    | otherwise = divisoresPositivos x (y-1)

esPrimo :: Int -> Bool
esPrimo x
    | length (divisoresPositivos x x) == 2 = True
    | otherwise = False

-- ej 3

inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1/x)

aEntero :: Either Int Bool -> Int
aEntero (Left x) = x
aEntero (Right y)
    | y = 1
    | otherwise = 0


--ej 4

limpiar :: String -> String -> String
limpiar _ "" = ""
limpiar a (x:xs)
    | pertenece x a = limpiar a xs
    | otherwise = x : limpiar a xs


pertenece :: Char -> String -> Bool
pertenece _ "" = False
pertenece a (x:xs)
    | a == x = True
    | a /= x = pertenece a xs


difPromedio :: [Float] -> [Float]
difPromedio [] = []
difPromedio a = restarLista a (promedio a)

restarLista :: [Float] -> Float -> [Float]
restarLista [] _ = []
restarLista (x:xs) a = (x-a) : restarLista xs a

promedio :: [Float] -> Float
promedio [] = 0
promedio a = (sumarElementos a) / (fromIntegral (length a))

sumarElementos :: [Float] -> Float
sumarElementos [] = 0
sumarElementos (x:xs) = x + sumarElementos xs


todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales [b] = True
todosIguales (a:b:xs)
    | a == b = todosIguales (b:xs)
    | otherwise = False


-- ej 5

data AB a = Nil | Bin (AB a) a (AB a)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB x = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin izq x der)
    | x = Bin (negacionAB izq) False (negacionAB der)
    | otherwise = Bin (negacionAB izq) True (negacionAB der)

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin izq x der) = x * productoAB izq * productoAB der
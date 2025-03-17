import System.Win32 (xBUTTON1)
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

valorAbsoluto :: Float -> Float
valorAbsoluto x
    | x >= 0 = x
    | otherwise = -x
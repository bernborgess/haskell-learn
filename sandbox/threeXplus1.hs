-- The famous 3x+1 problem
f :: Int -> Int
f x  | odd x = 3 * x + 1
     | even x = x `div` 2

-- Function that iterates on f until 1 is found
g :: Int -> [Int]
g 1 =  [1]
g x = x : g (f x)

-- Lets create a function that counts iterations of txp1
h :: Int -> Int
h 1 = 1
h x = 1 + h (f x)

-- the number of iterations and x
m :: Int -> (Int,Int)
m x = (x,h x)

-- Get the best
best :: [(Int,Int)] -> (Int,Int)
best = foldr (\(n,i) (m,j) -> if i > j then (n,i) else (m,j)) (0,0)

find :: [Int] -> (Int,Int)
find = best . map m


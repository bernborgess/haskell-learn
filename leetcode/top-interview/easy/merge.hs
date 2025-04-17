
divide :: [Int] -> ([Int],[Int])
divide [] = ([],[])
divide [x] = ([x],[])
divide (x:y:t) = let (l,r) = divide t in (x:l,y:r)

conquer :: [Int] -> [Int] -> [Int]
conquer [] r = r
conquer l [] = l
conquer (x:l) (y:r) = if x < y
        then x : conquer l (y:r)
        else y : conquer (x:l) r

mergesort :: [Int] -> [Int]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = 
    let (l,r) = divide xs
        sl = mergesort l
        sr = mergesort r
     in conquer sl sr

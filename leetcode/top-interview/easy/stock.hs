
-- Given a list of stock prices per day, answer what is the best possible profit of buying on a day and selling on a future day
stock :: [Int] -> Int
stock = go 0 9999999999999999
 where
   go :: Int -> Int -> [Int] -> Int
   go d _ [] = d
   go d m (p:ps) = go nd nm ps
    where
        nd = max d (p-m)
        nm = min m p
        
-- Use a fold
stock' :: [Int] -> Int
stock' = fst . foldl f (0,inf)
 where
   f :: (Int,Int) -> Int -> (Int,Int)
   f (d,m) p = (nd,nm)
    where
        nd = max d (p-m)
        nm = min m p

   inf = 9999999999999999



-- Followup: we want to know what DAY to buy and sell specifically. If no such day exists, return (-1,-1)
stockday :: [Int] -> (Int,Int)
stockday ps = if di /= -1 then (mi,di) else (-1,-1)
 where
   pis :: [(Int,Int)]
   pis = zip ([0..length ps]) ps

   go :: (Int,Int) -> (Int,Int) -> [(Int,Int)] -> (Int,Int)
   go (di,d) (mi,m) [] = (mi,di)
   go (di,d) (mi,m) ((pi,p):ps) = go nd nm ps
    where
        nd = if p-m > d then (pi,p-m) else (di,d) 
        nm = if p < m then (pi,p) else (mi,m)

   (mi,di) = go (-1,0) (-1,9999999999999999) pis



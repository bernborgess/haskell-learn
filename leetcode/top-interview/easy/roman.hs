
hs :: Char -> Int
hs 'I' = 1
hs 'V' = 5
hs 'X' = 10
hs 'L' = 50
hs 'C' = 100
hs 'D' = 500
hs 'M' = 1000
hs _ = 0

roman :: String -> Int
roman s = 
    let xs = map hs s
        go :: Int -> [Int] -> Int -> Int
        go pre [] acc = acc
        go pre (x:xs) acc =
             let k = if x > pre then x - 2 * pre else x
              in go x xs (k+acc)
        inf = 999999999999999999
     in go inf xs 0



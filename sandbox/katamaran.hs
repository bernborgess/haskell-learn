{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use max" #-}

sumMaxLen :: [Int] -> ((Int, Int), Int)
sumMaxLen [] = ((0, 0), 0)
sumMaxLen (y : ys) =
  let ((s, m), l) = sumMaxLen ys
   in ((s + y, if m < y then y else m), l + 1)
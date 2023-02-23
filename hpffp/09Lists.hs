-- Write your own `enumFromTo` definitions for the types
-- provided. Do not use ranges syntax to do so. It should
-- return the same results as if you did [start..stop]

enumFrom2 :: Enum a => a -> a -> [a]
enumFrom2 start stop
  | fromEnum start > fromEnum stop = []
  | otherwise = start : enumFrom2 (succ start) stop

-- enumFrom2 1 3
-- => [1,2,3]

-- checks if my `enumFrom2` funcion is behaving like builtin [start..stop]
check :: Enum a => a -> a -> Bool
check start stop = map fromEnum [start .. stop] == map fromEnum (enumFrom2 start stop)

-- iterates every pair in range to apply the check
verifyRange :: Enum a => a -> a -> Bool
verifyRange l r = all (uncurry check) xys
  where
    xys = [(x, y) | x <- [l .. r], y <- [l .. r]]

-- =========================================================================
-- Intermission Exercises
-- 1. Using `takeWhile` and `dropWhile`, write a function that takes a string
-- and returns a list of strings, using spaces to separate the elements of the
-- string into words, as in the following example:

-- Main> myWords "all i wanna do is have some fun"
-- ["all","i","wanna","do","is","have","some","fun"]

myWords :: String -> [String]
myWords "" = []
myWords text =
  let trim = dropWhile (== ' ') text
      fstw = takeWhile (/= ' ') trim
      rest = dropWhile (/= ' ') trim
   in fstw : myWords rest

-- 2. Next, write a function that takes a string and returns a list of strings,
-- using the newline separators to break up the string as in the following

firstSen = "Type Tyger, burning bright\n"

secondSen = "In the forests of the night\n"

thirdSen = "What immortal hand or eye\n"

fourthSen = "Could frame thy fearful symmetry?"

sentences =
  firstSen
    ++ secondSen
    ++ thirdSen
    ++ fourthSen

-- putStrLn sentences -- should print
-- Type Tyger, burning bright
-- In the forests of the night
-- What immortal hand or eye
-- Could frame thy fearful symmetry?

myLines :: String -> [String]
myLines "" = []
myLines text =
  let trim = dropWhile (== '\n') text
      fstl = takeWhile (/= '\n') trim
      rest = dropWhile (/= '\n') trim
   in fstl : myLines rest

shouldEqual =
  [ "Type Tyger, burning bright",
    "In the forests of the night",
    "What immortal hand or eye",
    "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $
    "Are they equal? "
      ++ show (myLines sentences == shouldEqual)

-- 3. Now let's look at what those two functions have in common.
-- Try writing a new function that parameterizes the character
-- you're breaking the string argument on and rewrite `myWords`
-- and `myLines` using it
mySplitOn :: Char -> String -> [String]
mySplitOn _ "" = []
mySplitOn c text =
  let trim = dropWhile (== c) text
      fstl = takeWhile (/= c) trim
      rest = dropWhile (/= c) trim
   in fstl : mySplitOn c rest

myWords' :: String -> [String]
myWords' = mySplitOn ' '

myLines' :: String -> [String]
myLines' = mySplitOn '\n'

-- ========================================================
-- ON LAZINESS AND STRICTNESS
-- 1.
l1 = [x ^ y | x <- [1 .. 5], y <- [2, undefined]]

-- fine
-- take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]

-- blows up
-- i3 = sum [1, undefined, 3]

-- fine
l4 = length [1, 2, undefined]

-- blows up (spine is undefined)
l5 = length $ [1, 2, 3] ++ undefined

-- fine
l6 = take 1 $ filter even [1, 2, 3, undefined]

-- blows up (whole list is evaluated)
l7 = take 1 $ filter even [1, 3, undefined]

-- fine
l8 = take 1 $ filter odd [1, 3, undefined]

-- fine
l9 = take 2 $ filter odd [1, 3, undefined]

-- blows up
l10 = take 3 $ filter odd [1, 3, undefined]

-- ? Is it in normal form?
-- For each expression, determine whether it's in:
-- 1. Normal Form, which implies weak head normal form
-- 2. Weak head normal form only; or
-- 3. neither.

-- ! Values in Haskell get reduced to weak head normal
-- ! form by default. By ‘normal form’ we mean that the
-- ! expression is fully evaluated. ‘

-- ! ‘Weak head normal form’ means the expression is
-- ! only evaluated as far as is necessary to reach a
-- ! data constructor.

{-
 Remember that an expression cannot be in normal form or weak
 head normal form if the outermost part of the expression isn’t a data
 constructor. It can’t be in normal form if any part of the expression is
 unevaluated
-}
{-
1. [1, 2, 3, 4, 5]
  => 1
2. 1 : 2 : 3 : 4 : _
  => 2
3. enumFromTo 1 10
  => 3
4. length [1, 2, 3, 4, 5]
  => 3
5. sum (enumFromTo 1 10)
  => 3
6. ['a'..'m'] ++ ['n'..'z']
  => 3
7. (_, 'b')
  => 2
-}

-- =================================================
-- 9.10 Filtering lists of values
-- 1. Given the above, how might we write a filter function that would
-- give us all the multiples of 3 out of a list from 1-30?
filter1 :: [Int] -> [Int]
-- filter1 xs = filter (\x -> x `mod` 3 == 0) xs
filter1 = filter $ (0 ==) . (`mod` 3)

-- 2. Recalling what we learned about function composition, how
-- could we compose the above function with the length function
-- to tell us *how many* multiples of 3 there are between 1 and 30?
multiples3 :: [Int] -> Int
multiples3 = length . filter1

-- 3. Next we’re going to work on removing all articles (’the’, ’a’, and
-- ’an’) from sentences. You want to get to something that works
-- like this:
-- Prelude> myFilter "the brown dog was a goof"
-- ["brown","dog","was","goof"]
noArticles :: String -> [String]
noArticles = filter (`notElem` ["the", "a", "an"]) . words

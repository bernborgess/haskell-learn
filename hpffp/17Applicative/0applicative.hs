{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$" #-}
{-# HLINT ignore "Use <$>" #-}
import Control.Applicative
import Control.Monad
import Data.Char
import Data.Monoid

-- ? 17.1 Applicative
-- In the previous chapters, we’ve seen two common algebras that are
-- used as typeclasses in Haskell. Monoid gives us a means of mashing
-- two values of the same type together. Functor, on the other hand, is
-- for function application over some structure we don’t want to have to
-- think about. Monoid’s core operation, mappend smashes the structures
-- together — when you mappend two lists, they become one list, so the
-- structures themselves have been joined. However, the core operation
-- of Functor, fmap applies a function to a value that is within some
-- structure while leaving that structure unaltered.
-- We come now to Applicative. Applicative is a monoidal functor. No,
-- no, stay with us. The Applicative typeclass allows for function
-- application lifted over structure (like Functor). But with Applicative the
-- function we’re applying is also embedded in some structure. Because
-- the function and the value it’s being applied to both have structure,
-- we have to smash those structures together. So, Applicative involves
-- monoids and functors. And that’s a pretty powerful thing.

{-
class Functor f => Applicative f where
  -- Embeds smth into structure
  pure :: a -> f a

  -- apply
  (<*>) :: f (a -> b) -> f a -> f b
  liftA :: Applicative f => (a -> b) -> f a -> f b
  liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
  ...
-}
-- Let’s review the difference between fmap and <*>:
-- fmap :: (a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b
-- ? The difference appears to be quite small and innocuous. We now have
-- ? an 𝑓 in front of our function (a -> b). But the increase in power
-- ! it introduces is profound.
-- fmap f x = pure f <*> x

-- 17.4 Applicative functors are monoidal functors
-- First let us notice something:
-- ($) :: (a -> b) -> a -> b
-- (<$>) :: (a -> b) -> f a -> f b
-- (<*>) :: f (a -> b) -> f a -> f b

-- Examples
ex1 = [(* 2), (* 3)] <*> [4, 5]

ex2 = Just (* 2) <*> Just 2

ex3 = Just (* 3) <*> Nothing

ex4 = Nothing <*> Just 4

-- Show me the monoids!
ex5 = ("Woo", (+ 1)) <*> (" Hoo!", 0)

ex6 = (Sum 2, (+ 1)) <*> (Sum 0, 0)

ex7 = (Product 3, (+ 9)) <*> (Product 2, 8)

ex8 = (All True, (+ 1)) <*> (All False, 0)

-- ? It doesn’t really matter what Monoid, we
-- just need some way of combining or choosing
-- our values.
-- Tuple Monoid and Applicative side by side
-- Squint if you can’t see it.

-- instance (Monoid a, Monoid b) => Monoid (a, b) where
--   mempty = (mempty, mempty)
--   (a, b) `mappend` (a', b') =
--     (a `mappend` a', b `mappend` b')

-- instance Monoid a => Applicative ((,) a) where
--   pure x = (mempty, x)
--   (u, f) <*> (v, x) =
--     (u `mappend` v, f x)

ex9 = (,) <$> [1, 2] <*> [3, 4]

-- first we fmap the (,) over the first list
-- [(1, ), (2, )] <*> [3, 4]

-- then we apply the first list
-- to the second
-- [(1,3),(1,4),(2,3),(2,4)]

-- The liftA2 function gives us another way to
-- write this, too:
-- Prelude> liftA2 (,) [1, 2] [3, 4]
-- [(1,3),(1,4),(2,3),(2,4)]

ex10 = (+) <$> [1, 2] <*> [3, 5]

-- first we fmap the (+) over the first list
-- [(+1), (+2)] <*> [3, 5]

-- then we apply the first list to the second
-- [4,6,5,7]

-- == liftA2 (+) [1,2] [3,5]

ex11 = max <$> [1, 2] <*> [1, 4]

-- first we fmap the max over the first list
-- [(\x -> max 1 x), (\x -> max 2 x)] <*> [1, 4]

-- then we apply the first list to the second
-- [1,4,2,4]
-- == liftA2 max [1,2] [1,4]

cap (x : xs) = toUpper x : xs

-- Using lookup
-- dictionary = fromList [(3, "hello"), (4, "dolly")]

-- ahn = Data.Map.lookup 3 dictionary

f x = lookup x [(3, "hello"), (4, "julie"), (5, "kbai")]

g y = lookup y [(7, "sup?"), (8, "chris"), (9, "aloha")]

h z = lookup z [(2, 3), (5, 6), (7, 8)]

m x = lookup x [(4, 10), (8, 13), (1, 9001)]

-- Prelude> f 3
-- Just "hello"
-- Prelude> g 8
-- Just "chris"
-- Prelude> (++) <$> f 3 <*> g 7
-- Just "hellosup?"
-- Prelude> (+) <$> h 5 <*> m 1
-- Just 9007
-- Prelude> (+) <$> h 5 <*> m 6
-- Nothing

-- Applicative with IO
l1 x = lookup x [(3, "hello"), (4, "dolly")]

l2 x = lookup x [(6, "so nice to have you"), (7, "back where you belong")]

fact x = lookup x [(0, 1), (1, 1), (2, 2), (3, 6)]

square x = lookup x [(0, 0), (1, 1), (2, 4), (3, 9)]

aio = (++) <$> getLine <*> getLine

ad3 x y = (x + y +)

-- Playing with a Stack
newtype Stack a = Stack [a]

instance Foldable Stack where
  foldr f acc (Stack xs) = foldr f acc xs

push :: Stack a -> a -> Stack a
push (Stack xs) x = Stack $ x : xs

pop :: Stack a -> Maybe (a, Stack a)
pop (Stack (x : xs)) = Just (x, Stack xs)
pop _ = Nothing

isCmd :: String -> Bool
isCmd s = fs `elem` ["push", "pop"]
  where
    fs = head $ words s

st = Stack []

stackle :: Stack Integer -> IO ()
stackle sa = do
  opt <- getLine
  if isCmd opt
    then do
      if opt == "pop"
        then do
          putStr "pop"
          let ba = pop sa
          case ba of
            Nothing -> putStr "Empty!"
            Just (x, ns) -> do
              putStr $ show x
              stackle ns
        else do
          putStr "push"
          x <- readLn :: IO Integer
          let nsa = push sa x
          stackle nsa
    else putStr "over"

-- stackle

-- return ()

-- Exercise
-- Given the function and values provided, use (<$>) from Functor,
-- (<*>) and pure from the Applicative typeclass to fill in missing bits
-- of the broken code to make it work.
-- 1.
e1 = const <$> Just "Hello" <*> Just "World"

-- 2.
e2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> pure [1, 2, 3]

-- Applicative laws
-- 1. Identity
id1 v = (pure id <$> v) == v

-- 2. Composition
comp1 u v w = e1 == e2
  where
    -- No suprises in function application composition
    e1 = pure (.) <*> u <*> v <*> w
    e2 = u <*> (v <*> w)

comp2 = pure (.) <*> [(+ 1)] <*> [(* 2)] <*> [1, 2, 3]

-- 3. Homomorphism
-- A homomorphism is a structure-preserving map between two
-- categories. The effect of applying a function that is embedded
-- in some structure to a value that is embedded in some structure
-- should be the same as applying a function to a value without
-- affecting any outside structure:
-- e1 = pure f <*> pure x :: ap b
-- ===
-- e2 = pure (f x)

homo1 = pure (+ 1) <*> pure 1 :: Maybe Int

homo2 = pure (+ 1) <*> pure 1 :: [Int]

homo3 = pure (+ 1) <*> pure 1 :: Either a Int

-- The general idea of the homomorphism law is that applying the
-- function doesn’t change the structure around the values

-- 4. Interchange
-- u <*> pure y = pure ($ y) <*> u

int1 = e1 == e2
  where
    e1 = Just (+ 2) <*> pure 2
    e2 = pure ($ 2) <*> Just (+ 2)

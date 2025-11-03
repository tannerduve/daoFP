{-# LANGUAGE GADTs #-}
module Ch07.Recursion where

data Nat where
    Z :: Nat
    S :: Nat -> Nat

rec :: a -> (a -> a) -> Nat -> a 
rec base _ Z = base
rec base step (S n') = step (rec base step n')

plus :: Nat -> Nat -> Nat 
plus n = rec n S 

plus' :: Nat -> Nat -> Nat 
plus' n m = case m of 
    Z -> n 
    S n' -> S (plus' n' m)

natToInt :: Nat -> Int
natToInt = rec 0 inc where 
    inc i = i + 1

addinit :: Nat -> Nat
addinit n = n

addstep :: (Nat -> Nat) -> (Nat -> Nat)
addstep f = S . f

add :: Nat -> (Nat -> Nat)
add n = rec (addinit n) (addstep S)

data List a where
    Nil :: List a
    Cons :: (a, List a) -> List a

recList :: c -> ((a, c) -> c) -> List a -> c 
recList base _ Nil = base 
recList base step (Cons(hd, tl)) = step (hd, recList base step tl)

foldrC :: (a -> c -> c) -> c -> [a] -> c
foldrC _ base [] = base 
foldrC step base (hd : tl) = step hd (foldrC step base tl)

sum :: [Nat] -> Nat 
sum = foldr plus Z

h :: [a] -> Maybe a 
h = foldrC step base where
    base = Nothing 
    step a Nothing = Just a
    step _ (Just b) = Just b

third :: [a] -> Maybe a
third xs = foldr step base xs 2
  where
    base _         = Nothing
    step a _ 0     = Just a
    step _ f k     = f (k - 1)

mapList :: (a -> b) -> List a -> List b 
mapList f = recList base step where 
    base = Nil
    step(a, l) = Cons(f a, l)



    
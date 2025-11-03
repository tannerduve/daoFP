{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Ch08.Functors where

import Data.Bifunctor (Bifunctor(..))
import Data.Functor.Contravariant (Contravariant(..)) 


data WithInt a = WithInt a Int

instance Functor WithInt where 
    fmap f (WithInt elt z) = WithInt (f elt) z


newtype Identity a = Identity a

instance Functor Identity where 
    fmap f (Identity elt) = Identity (f elt)

data Constant c a = Constant c

instance Functor (Constant c) where 
    fmap _ (Constant elt) = Constant elt
    
data MoreThanA a b = More a (Maybe b)

instance Functor (MoreThanA a) where 
    fmap f (More am bm) = More am (fmap f bm)

instance Bifunctor MoreThanA where 
    bimap f g (More am bm) = More (f am) (fmap g bm)

newtype Predicate a = Predicate (a -> Bool)

instance Contravariant Predicate where
    contramap f (Predicate p) = Predicate (p . f)

newtype Compose f g a = Compose (f (g a))

instance (Functor f, Functor g) => Functor (Compose f g) where 
    fmap h (Compose fga) = Compose (fmap (fmap h) fga)
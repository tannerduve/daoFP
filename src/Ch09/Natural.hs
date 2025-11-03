{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
module Ch09.Natural where

import Data.Functor.Compose ( Compose(..) )
import Data.Functor.Contravariant (Contravariant(..))


import Data.Kind   
import Data.Void (Void, absurd)

data Natural :: (Type -> Type) -> (Type -> Type) -> Type where 
    Natural :: (forall a. f a -> g a) -> Natural f g

id :: forall a. a -> a 
id x = x

type NaturalT f g = forall a. f a -> g a

oneWay :: forall f g a b. (Functor f, Functor g) => NaturalT f g ->
    (a -> b) -> f a -> g b 
oneWay alpha h = fmap @g h . alpha @a

otherWay :: forall f g a b. (Functor f, Functor g) => NaturalT f g ->
    (a -> b) -> f a -> g b 
otherWay alpha h = alpha @b . fmap @f h

safeHead :: NaturalT [] Maybe
safeHead [] = Nothing 
safeHead (h : _) = Just h

reverseT :: NaturalT [] []
reverseT [] = []
reverseT (hd : tl) = reverseT tl ++ [hd]

-- Give Compose an operator alias
type (:.:) = Compose
infixr 9 :.:

pattern Comp :: f (g a) -> (f :.: g) a
pattern Comp x = Compose x
{-# COMPLETE Comp #-}

betaAlpha
  :: (Functor g, Functor f, Functor g', Functor f')
  => NaturalT f f' -> NaturalT g g'
  -> NaturalT (g :.: f) (g' :.: f')
betaAlpha alpha beta (Comp gfa) = Comp (beta (fmap alpha gfa))

betaAlpha2
  :: (Functor g, Functor f, Functor g', Functor f')
  => NaturalT f f' -> NaturalT g g'
  -> NaturalT (g :.: f) (g' :.: f')
betaAlpha2 alpha beta (Comp gfa) = Comp (fmap alpha (beta gfa))

-- reverse :: NaturalT f = [] f' = []
-- safeHead :: NaturalT g = Maybe g' = []
-- safeHeadReverse :: NaturalT ([] :.: []) ([] :.: Maybe) 
-- reverseSafeHead :: NaturalT (Maybe :.: []) ([] :.: [])

safeHeadReverse :: NaturalT ([] :.: []) ([] :.: Maybe) 
safeHeadReverse = betaAlpha safeHead reverseT

safeHeadReverse' :: NaturalT (Compose [] []) (Compose [] Maybe) 
safeHeadReverse' (Compose l) = Compose { getCompose = reverse (fmap safeHead l) }

betaF :: forall g f g' x. NaturalT g g' -> g (f x) -> g' (f x) 
betaF beta = beta

betaF' :: forall g f g'. NaturalT g g' -> NaturalT (g :.: f) (g' :.: f)
betaF' beta (Compose gfa ) = Compose { getCompose = beta gfa }

gAlpha :: forall f f' g x. (Functor g, Functor f, Functor f') => NaturalT f f' -> g (f x) -> g (f' x)
gAlpha = fmap

gAlpha' :: forall f f' g. (Functor g, Functor f, Functor f') => NaturalT f f' -> NaturalT (g :.: f) (g :.: f')
gAlpha' alpha (Compose gfa) = Compose { getCompose = fmap alpha gfa }

hBetaF :: forall f g g' h x. (Functor g, Functor g', Functor f, Functor h) => NaturalT g g' -> h (g (f x)) -> h (g' (f x)) 
hBetaF = fmap


-- some gaps

-- Yoneda

-- forall x. (a -> x) -> f x.

yoneda :: Functor f => (forall x. (a -> x) -> f x) -> f a 
yoneda g = g Prelude.id

yoneda' :: Functor f => f a -> (forall x. (a -> x) -> f x)
yoneda' fa h = fmap h fa 

coyoneda :: Contravariant f => (forall x. (x -> a) -> f x) -> f a
coyoneda g = g Prelude.id 

coyoneda' :: Contravariant f => f a -> (forall x. (x -> a) -> f x)
coyoneda' fa h = contramap h fa

-- Representable functors

class Representable f where 
  type Key f :: Type
  tabulate :: (Key f -> x) -> f x 
  index :: f x -> (Key f -> x)

data Stream a = Stm a (Stream a)

data Nat = Z | S Nat
  deriving (Eq, Show)

instance Representable Stream where 
  type Key Stream = Nat
  tabulate g = tab Z where 
    tab n = Stm (g n) (tab (S n))
  index stm n = ind n stm where 
    ind Z (Stm x _) = x
    ind (S n') (Stm _ st) = ind n' st

data Pair x = Pair x x

instance Representable Pair where 
  type Key Pair = Bool 
  tabulate g = Pair (g True) (g False)
  index (Pair x' _) True = x' 
  index (Pair _ y') False = y'

data Unit a = U

instance Functor Unit where
  fmap _ U = U

instance Representable Unit where 
  type Key Unit = Void
  tabulate _ = U 
  index _ = absurd
  



module Ch06.Function where

curry :: ((c, a) -> b) -> (c -> (a -> b))
curry f ce ae = f(ce, ae)

uncurry :: (c -> (a -> b)) -> ((c, a) -> b)
uncurry f (ce, ae) = f ce ae

mapOut :: (a -> c, b -> c) -> (Either a b -> c)
mapOut (f, _) (Left ae) = f ae 
mapOut (_, g) (Right be) = g be

mapIn :: (c -> a, c -> b) -> (c -> (a, b))
mapIn (f, g) ce = (f ce, g ce)

biMap :: (a -> a') -> (b -> b') -> Either a b -> Either a' b' 
biMap f _ (Left a) = Left (f a)
biMap _ g (Right b) = Right (g b)

dist :: Either (b, a) (c, a) -> (Either b c, a)
dist (Left (b, a)) = (Left b, a)
dist (Right (c, a)) = (Right c, a)

boolIso :: (Bool, a) -> Either a a 
boolIso (_, a) = Left a 

boolIsoInv :: Either a a -> (Bool, a)
boolIsoInv (Left a) = (True, a)
boolIsoInv (Right a) = (False, a)

{-# LANGUAGE GADTs #-}
module Ch04.Sum where
import Data.Void


-- data Boolo where
--     True :: Boolo
--     False :: Boolo

not :: Bool -> Bool 
not b = if b then False else True

id :: Bool -> Bool 
id b = if b then True else False 

true :: Bool -> Bool 
true b = if b then True else True 

false :: Bool -> Bool 
false b = if b then False else False

data RGB = Red | Green | Blue

c :: RGB 
c = Blue

addZero :: Either a Void -> a 
addZero (Left ae) = ae 
addZero (Right v) = absurd v 

addZeroInv :: a -> Either a Void 
addZeroInv = Left

comm :: Either a b -> Either b a 
comm (Left ae) = Right ae
comm (Right be) = Left be

maybeAB :: Either b (a, b) -> (Maybe a, b)
maybeAB (Left be) = (Nothing, be) 
maybeAB (Right (ae, be)) = (Just ae, be)
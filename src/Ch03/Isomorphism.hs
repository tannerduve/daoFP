module Ch03.Isomorphism where
-- type alias (type equality is evil)
type MyTemperature = Int 

-- type isomorphism
newtype MyTemp = Temp {toInt :: Int}

invert :: MyTemp -> MyTemp 
invert = Temp . negate . toInt


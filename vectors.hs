data Vector a = Vector a a a deriving (Show)

vectPlus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vectPlus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)

scalarMult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarMult` (Vector l m n) = (i*l) + (j*m) + (k*n)

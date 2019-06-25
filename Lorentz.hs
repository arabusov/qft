module Lorentz where

    import Data.Complex

    data Vector a = Vector a a a deriving (Show, Eq)

    vplus :: (Num t) => Vector t -> Vector t -> Vector t
    (Vector x y z) `vplus` (Vector a b c) = Vector (x+a) (y+b) (z+c)

    vmult :: (Num t) => t -> Vector t -> Vector t
    lambda `vmult` (Vector x y z) = Vector (x*lambda) (y*lambda) (z*lambda)

    scalarMult :: Vector (Complex Double) -> Vector (Complex Double) -> Complex Double
    (Vector x1 y1 z1) `scalarMult` (Vector x2 y2 z2) = conjugate (x1)*x2 + conjugate (y1) * y2 + conjugate (z1)*z2

    data FourVector = FourVector { t :: Complex Double
                             , r :: Vector (Complex Double)
                             } deriving (Show, Eq)

 

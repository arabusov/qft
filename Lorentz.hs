module Lorentz where

    import Data.Complex

    data Vector a = Vector a a a deriving (Show, Eq)
    newtype HermVector = HermVector {getVector :: Vector (Complex Double)} deriving (Show, Eq)


    
    data FourVector = FourVector { t :: Complex Double
                             , r :: HermVector
                             } deriving (Show, Eq)

    class CanCdot a where
        cdot :: a -> a -> Complex Double
        vmult :: (Num t) => t -> a -> a
        vplus :: a -> a -> a
    instance CanCdot HermVector where
        (HermVector (Vector x1 y1 z1)) `cdot` (HermVector (Vector x2 y2 z2)) = conjugate (x1) * x2 + conjugate (y1) * y2 + conjugate (z1) * z2
        (HermVector x y z) `vplus` (Vector a b c) = Vector (x+a) (y+b) (z+c)
        lambda `vmult` (HermVector x y z) = HermVector (x*lambda) (y*lambda) (z*lambda)
    instance CanCdot FourVector where
        (FourVector t1 r1) `cdot` (FourVector t2 r2) = conjugate (t1) * t2 - r1 `cdot` r2



 

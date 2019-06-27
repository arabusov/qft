module Lorentz where

    import Data.Complex

    data Vector a = Vector a a a deriving (Show, Eq)
    newtype HermVector = HermVector {getVector :: Vector (Complex Double)} deriving (Show, Eq)
    toHermVector :: Vector Double -> HermVector
    toHermVector (Vector x y z) = HermVector (Vector (x :+ 0.0) (y :+ 0.0) (z :+ 0.0))
    
    data FourVector = FourVector { t :: Complex Double
                             , r :: HermVector
                              } deriving (Show, Eq)
    toFourVector :: Double -> (Vector Double) -> FourVector
    toFourVector mass pvec = FourVector t (toHermVector pvec) where
        t = sqrt ((mass*mass :+ 0.0) + ((toHermVector pvec) `cdot` (toHermVector pvec)))
    class CanCdot a where
        cdot :: a -> a -> Complex Double
        vmult :: (Complex Double) -> a -> a
        vplus :: a -> a -> a
        ith :: a -> Int -> Complex Double
    instance CanCdot HermVector where
        (HermVector (Vector x1 y1 z1)) `cdot` (HermVector (Vector x2 y2 z2)) =
            (conjugate (x1) * x2) + (conjugate (y1) * y2) + (conjugate (z1) * z2)
        (HermVector (Vector x y z)) `vplus` HermVector (Vector a b c) = HermVector (Vector (x+a) (y+b) (z+c))
        lambda `vmult` (HermVector (Vector x y z)) = HermVector (Vector (x*lambda) (y*lambda) (z*lambda))
        ith (HermVector (Vector x y z)) n | n == 0  = x
                                          | n == 1  = y
                                          | n == 2  = z
                                          | otherwise = error "index out of range."
    instance CanCdot FourVector where
        (FourVector t1 r1) `vplus` (FourVector t2 r2) = FourVector (t1+t2) (r1 `vplus` r2)
        lambda `vmult` (FourVector t1 r1) = FourVector (lambda*t1) (lambda `vmult` r1)
        (FourVector t1 r1) `cdot` (FourVector t2 r2) = conjugate (t1) * t2 - r1 `cdot` r2
        ith (FourVector t r) n | n == 0 = t
                               | otherwise = ith r (n-1)


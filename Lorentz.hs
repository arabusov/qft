{-|
Module          : Lorentz
Description     : Haskell implementation of lorentz group transformations
License         : GPL-3
Maintainer      : arabusov@gmail.com
Stability       : experimental

This module provides an interface and implementation
for basic definition of 3D and 4D vectors
and to some transformation functions. The main idea is to make it easy
for simple numerical theoretical calculations in QFT (mostly tree-level
diagrams, phase space relations), which is quite useful for experimental
particle physics (or high energy physics).
You must take into account, that all this stuff is done for fun, therefore
I provide this module without any warranty, see GPL-3 description.
-}
module Lorentz (
               -- * Classes
                 VectorSpaceClass (..)
               -- * Types
               -- ** Data Types
               , Vector(..)
               , HermVector(..)
               , FourVector(..)
               -- * Functions
               , toHermVector
               , toFourVector
               , interval2
               , interval
               ) where
    import Data.Complex
    -- | Basic 3D vector
    data Vector a = Vector a a a deriving (Show, Eq)
    -- | Specific implementation of 3D vector in Hermitian vector space
    -- note, than x, y and z \belong Complex field
    newtype HermVector = HermVector {getVector :: Vector (Complex Double)} deriving (Show, Eq)
    -- | 'toHermVector' attaches zero imaginary part to all components of
    -- 'Vector Double'
    toHermVector :: Vector Double -> HermVector
    toHermVector (Vector x y z) = HermVector (Vector (x :+ 0.0) (y :+ 0.0) (z :+ 0.0))
    -- | 4D pseudo-vector (Minkovsky space) 
    data FourVector = FourVector { t :: Complex Double
                             , r :: HermVector
                              } deriving (Show, Eq)
    -- | 'toFourVector' constructs FourVector from known mass and 'Vector
    -- Double'
    toFourVector :: Double -> (Vector Double) -> FourVector
    toFourVector mass pvec = FourVector t (toHermVector pvec) where
        t = sqrt ((mass*mass :+ 0.0) + ((toHermVector pvec) `cdot` (toHermVector pvec)))
    -- | 'interval2' returns mass squared of gived four-momentum
    interval2 :: FourVector -> (Complex Double)
    interval2 fv = fv `cdot` fv
    -- | 'interval' returns mass of gived four-momentum
    interval :: FourVector -> (Complex Double)
    interval fv = sqrt (interval2 fv)
    -- | 'VectorSpaceClass' gives an interface for basic vector operations, such
    -- as plus, multiplication to scalar and scalar product
    class VectorSpaceClass a where
        -- | 'cdot' returns scalar product
        cdot :: a -> a -> Complex Double
        -- | 'vmult' returns multiplication of a vector to a scalar
        vmult :: (Complex Double) -> a -> a
        -- | vplus returns sum of two vectors
        vplus :: a -> a -> a
        -- | ith returns ith component of the vector, [0..2] in 3D, [0..3] in
        -- pseudo-4D
        ith :: a -> Int -> Complex Double
        -- | rotate around z axis by some angle in radians
        rotateOfZ :: a -> Double -> a
        -- | rotate around x axis by some angle in radians
        rotateOfX :: a -> Double -> a
        -- | rotate around y axis by some angle in radians
        rotateOfY :: a -> Double -> a
        rho :: a -> Double
    instance VectorSpaceClass HermVector where
        (HermVector (Vector x1 y1 z1)) `cdot` (HermVector (Vector x2 y2 z2)) =
            (conjugate (x1) * x2) + (conjugate (y1) * y2) + (conjugate (z1) * z2)
        (HermVector (Vector x y z)) `vplus` HermVector (Vector a b c) = HermVector (Vector (x+a) (y+b) (z+c))
        lambda `vmult` (HermVector (Vector x y z)) = HermVector (Vector (x*lambda) (y*lambda) (z*lambda))
        ith (HermVector (Vector x y z)) n | n == 0  = x
                                          | n == 1  = y
                                          | n == 2  = z
                                          | otherwise = error "index out of range."
        rotateOfZ (HermVector (Vector x y z)) psi = HermVector  (Vector x' y' z) where
            x' = x * (cos (psi) :+ 0.0) - y * (sin (psi) :+ 0.0)
            y' = x * (sin (psi) :+ 0.0) + y * (cos (psi) :+ 0.0)
        rotateOfX (HermVector (Vector x y z)) psi = HermVector  (Vector x y' z') where
            y' = y * (cos (psi) :+ 0.0) - z * (sin (psi) :+ 0.0)
            z' = y * (sin (psi) :+ 0.0) + z * (cos (psi) :+ 0.0)
        rotateOfY (HermVector (Vector x y z)) psi = HermVector  (Vector x' y z') where
            z' = z * (cos (psi) :+ 0.0) - x * (sin (psi) :+ 0.0)
            x' = z * (sin (psi) :+ 0.0) + x * (cos (psi) :+ 0.0)
        rho hermvec = magnitude (sqrt (hermvec `cdot` hermvec))
    instance VectorSpaceClass FourVector where
        (FourVector t1 r1) `vplus` (FourVector t2 r2) = FourVector (t1+t2) (r1 `vplus` r2)
        lambda `vmult` (FourVector t1 r1) = FourVector (lambda*t1) (lambda `vmult` r1)
        (FourVector t1 r1) `cdot` (FourVector t2 r2) = conjugate (t1) * t2 - r1 `cdot` r2
        ith (FourVector t r) n | n == 0 = t
                               | otherwise = ith r (n-1)
        rotateOfZ (FourVector t r) psi = FourVector t (rotateOfZ r psi)
        rotateOfX (FourVector t r) psi = FourVector t (rotateOfX r psi)
        rotateOfY (FourVector t r) psi = FourVector t (rotateOfY r psi)
        rho (FourVector t r) = rho r


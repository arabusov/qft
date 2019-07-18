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
               , threeVector
               , hermVector
               , toFourVector
               , interval2
               , interval
               , boostX
               , boostY
               , boostZ
               , boostInDirection
               ) where
    import Data.Complex
    -- | Basic 3D vector
    data Vector a = Vector a a a deriving (Show, Eq)
    -- | Specific implementation of 3D vector in Hermitian vector space
    -- note, than x, y and z \belong Complex field
    newtype HermVector = HermVector {getHermVector :: Vector (Complex Double)} deriving (Show, Eq)
    -- | Also a specific instance for 3D vector, but in Orthogonal space
    newtype ThreeVector = ThreeVector {getRealVector :: Vector (Double)} deriving (Show, Eq)
    -- | 'toHermVector' attaches zero imaginary part to all components of
    -- 'Vector Double'
    toHermVector :: Vector Double -> HermVector
    toHermVector (Vector x y z) = HermVector (Vector (x :+ 0.0) (y :+ 0.0) (z :+ 0.0))
    -- | "Easy" function to create a HermVector with real components
    threeVector :: Double -> Double -> Double -> HermVector
    threeVector x y z = toHermVector (Vector x y z)
    -- | Also "Easy" function to make a HermVector from complex components
    -- | this function is useful to avoid using a mess construction such as
    -- | HermVector (Vector x y z), you can just write hermVector x y z instead
    hermVector :: (Complex Double) -> (Complex Double) -> (Complex Double) -> HermVector
    hermVector x y z = HermVector (Vector x y z)
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
    -- | 'boost1D' helps to unify boostX, boostY and boostZ functions
    boost1D :: (Complex Double) -> (Complex Double) -> Double -> ((Complex Double), (Complex Double))
    boost1D t x beta = (t', x') where
                                    beta' = (beta :+ 0)
                                    gamma = 1.0/sqrt(1-beta'**2)
                                    t' = gamma * t + gamma * beta' * x
                                    x' = gamma * x + gamma * beta' * t
    -- | 'boostX' returns FourVector, boosted along the x axis
    boostX :: FourVector -> Double -> FourVector
    boostX (FourVector t (HermVector (Vector x y z))) beta =
        FourVector t' (HermVector (Vector x' y z)) where
        (t', x') = boost1D t x beta
    -- | 'boostY' returns FourVector, boosted along the y axis
    boostY :: FourVector -> Double -> FourVector
    boostY (FourVector t (HermVector (Vector x y z))) beta =
        FourVector t' (HermVector (Vector x y' z)) where
        (t', y') = boost1D t y beta
    -- | 'boostZ' returns FourVector, boosted along the z axis
    boostZ :: FourVector -> Double -> FourVector
    boostZ (FourVector t (HermVector (Vector x y z))) beta =
        FourVector t' (HermVector (Vector x y z')) where
        (t', z') = boost1D t z beta
    -- | 'boostInDirection' returns a boosted in a certain 3D direction 4D vector
    boostInDirection :: FourVector -> Double -> (Vector Double) -> FourVector
    boostInDirection (FourVector t (HermVector (Vector x y z))) beta' (Vector nnx nny nnz) =
        let norm = sqrt (nnx**2 + nny**2 + nnz**2)
            beta = (beta' :+ 0)
            nx = ((nnx / norm) :+ 0)
            ny = ((nnx / norm) :+ 0)
            nz = ((nnz / norm) :+ 0)
            gamma = (1.0 / sqrt (1-beta**2))
            t' = gamma*t + gamma*beta*nx*x + gamma*beta*ny*y + gamma*beta*nz*z
            x' = gamma*beta*nx*t + (1+(gamma-1)*(nx**2))*x + (gamma-1)*nx*ny*y + (gamma-1)*nx*nz*z
            y' = gamma*beta*ny*t + (1+(gamma-1)*(ny**2))*y + (gamma-1)*ny*nx*x + (gamma-1)*ny*nz*z
            z' = gamma*beta*nz*t + (1+(gamma-1)*(nz**2))*z + (gamma-1)*nz*nx*x + (gamma-1)*ny*nz*y
        in FourVector t' (HermVector (Vector x' y' z'))
    -- | 'VectorSpaceClass' gives an interface for basic vector operations, such
    -- as plus, multiplication to scalar and scalar product
    class VectorSpaceClass a where
        -- | 'cdot' returns scalar product
        cdot :: a -> a -> Complex Double
        -- | 'vmult' returns multiplication of a vector to a scalar
        vmult :: (Complex Double) -> a -> a
        -- | vplus returns sum of two vectors
        vplus :: a -> a -> a
        -- | vminus returns diff of two vectors
        vminus :: a -> a -> a
        -- | veq returns if vectors are equal or not with predefined precision
        veq :: a -> a -> Double -> Bool
        -- | ith returns ith component of the vector, [0..2] in 3D, [0..3] in
        -- pseudo-4D
        ith :: a -> Int -> Complex Double
        -- | rotate around z axis by some angle in radians
        rotateOfZ :: a -> Double -> a
        -- | rotate around x axis by some angle in radians
        rotateOfX :: a -> Double -> a
        -- | rotate around y axis by some angle in radians
        rotateOfY :: a -> Double -> a
        -- | 'rho' returns the absolute value of spatial part of vector
        rho :: a -> Double
        -- | 'theta' returns theta in spherical coordinates 
        theta :: a -> Double
        -- | 'phi' returns phi in spherical coordinates
        phi :: a -> Double
    instance VectorSpaceClass HermVector where
        (HermVector (Vector x1 y1 z1)) `cdot` (HermVector (Vector x2 y2 z2)) =
            (conjugate (x1) * x2) + (conjugate (y1) * y2) + (conjugate (z1) * z2)
        (HermVector (Vector x y z)) `vplus` HermVector (Vector a b c) = HermVector (Vector (x+a) (y+b) (z+c))
        (HermVector (Vector x y z)) `vminus` HermVector (Vector a b c) = HermVector (Vector (x-a) (y-b) (z-c))
        (HermVector (Vector x y z) `veq` HermVector (Vector a b c)) epsilon = 
               (magnitude (x-a) < epsilon)
            && (magnitude (y-b) < epsilon)
            && (magnitude (z-c) < epsilon)
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
        phi (HermVector (Vector x y _)) | magnitude (x) == 0 && magnitude (y) == 0 = 0.0
                                        | magnitude (x) == 0 && realPart y >  0 = pi / 2.0
                                        | magnitude (x) == 0 && realPart y <  0 = - pi / 2.0
                                        | realPart y > 0 = atan (realPart y/realPart x)
                                        | realPart y < 0 = atan (realPart y/realPart x) - pi 
        theta (HermVector (Vector x y z)) = acos (zz/rho (HermVector (Vector x y z))) where
                                            zz = realPart z
    instance VectorSpaceClass FourVector where
        (FourVector t1 r1) `vplus` (FourVector t2 r2) = FourVector (t1+t2) (r1 `vplus` r2)
        (FourVector t1 r1) `vminus` (FourVector t2 r2) = FourVector (t1-t2) (r1 `vminus` r2)
        ( (FourVector t1 r1) `veq` (FourVector t2 r2)) epsilon = 
               (r1 `veq` r2) epsilon
            && magnitude (t1-t2) < epsilon
        lambda `vmult` (FourVector t1 r1) = FourVector (lambda*t1) (lambda `vmult` r1)
        (FourVector t1 r1) `cdot` (FourVector t2 r2) = conjugate (t1) * t2 - r1 `cdot` r2
        ith (FourVector t r) n | n == 0 = t
                               | otherwise = ith r (n-1)
        rotateOfZ (FourVector t r) psi = FourVector t (rotateOfZ r psi)
        rotateOfX (FourVector t r) psi = FourVector t (rotateOfX r psi)
        rotateOfY (FourVector t r) psi = FourVector t (rotateOfY r psi)
        rho (FourVector t r) = rho r
        theta (FourVector t r) = theta r
        phi (FourVector t r) = phi r


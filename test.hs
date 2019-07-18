import Lorentz
import Data.Complex
import Data.List

main = do
    let a = hermVector (2.0:+0.0) (3.0:+0.0) (4.0:+5.0)
    let zeroV = threeVector 0.0 0.0 0.0
    let mpi = 0.139
    let pv = Vector 0.3 0.2 0.5
    let ppi = toFourVector mpi pv
    let 
        psi = 3.1
        ppirot = rotateOfZ ppi psi
        ppirotback = rotateOfZ ppirot (-1.0 * psi)
    let
        beta = 0.5
        ppiboosted = boostX ppi beta
        ppiboostedback = boostX ppiboosted (-1.0 * beta)
    let
        betaX = 0.2
        ppiboostX = boostX ppi betaX
        directionX = (Vector 1.0 0.0 0.0)
        ppiboostDirX = (boostInDirection ppi betaX directionX)
    let
        betaY = 0.3
        ppiboostY = boostY ppi betaY
        directionY = (Vector 0.0 1.0 0.0)
        ppiboostDirY = (boostInDirection ppi betaY directionY)
    let
        betaZ = (-0.4)
        ppiboostZ = boostZ ppi betaZ
        directionZ = (Vector 0.0 0.0 1.0)
        ppiboostDirZ = (boostInDirection ppi betaZ directionZ)
    let
        precision = 10.0^^(-15)
    let testli = [ a `cdot` a == (54.0 :+ 0)
                 , a `cdot` zeroV == 0.0 :+ 0.0
                 , (0.0:+0.0) `vmult` a == zeroV
                 , a `vplus` zeroV == a
                 , (((14.0:+(-23.9)) `vmult` zeroV) `veq` zeroV) precision
                 , magnitude((ppi `cdot` ppi) - (mpi*mpi :+ 0)) < 10.0^^(-16)
                 , rho ppi == rho (rotateOfZ ppi 1.0) 
                 , rho ppi == rho (rotateOfX ppi 2.3)
                 , rho ppi == rho (rotateOfY ppi 3.4)
                 , (ppiboostedback `veq` ppi) precision
                 , (ppirotback `veq` ppi) precision
                 , (ppiboostX `veq` ppiboostDirX) precision
                 , (ppiboostY `veq` ppiboostDirY) precision
                 , (ppiboostZ `veq` ppiboostDirZ) precision
                 , epsilon 3 2 1 0 == 1
                 , epsilon 2 0 3 1 == -1
                 ]
    let enumeratedList = zip testli [1..length testli]
    putStr (concat (map (\ (x,y) -> if x then show y ++ ": Ok.\n"
                          else show y ++ ". Fail.\n") enumeratedList))
    putStrLn ("All test passed?  " ++ (if 1==product [if x then 1 else 0 | x <- testli] then "Yes." else "No."))

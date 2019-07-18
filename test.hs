import Lorentz
import Data.Complex

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
        direction = (Vector 1.0 0.0 0.0)
        ppiboostDir = (boostInDirection ppi betaX direction)
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
                 , (ppiboostX `veq` ppiboostDir) precision
                 ]
    putStrLn ("All test passed?  " ++ show (1==product [if x then 1 else 0 | x <- testli]))
    let enumeratedList = zip testli [1..length testli]
    print (map (\ (x,y) -> if x then show y ++ ": Ok."
                          else show y ++ ". Fail.") enumeratedList)
    print (ppiboostDir `vminus` ppiboostX)

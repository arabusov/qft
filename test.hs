import Lorentz
import Data.Complex

main = do
    let a = HermVector (Vector (2.0:+0.0) (3.0:+0.0) (4.0:+5.0))
    let zeroV = HermVector (Vector (0.0:+0.0) (0.0:+0.0) (0.0:+0.0))
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
        precision = 10.0^^(-15)
    let testli = [ a `cdot` a == (54.0 :+ 0)
                 , a `cdot` zeroV == 0.0 :+ 0.0
                 , (0.0:+0.0) `vmult` a == zeroV
                 , a `vplus` zeroV == a
                 , magnitude((ppi `cdot` ppi) - (mpi*mpi :+ 0)) < 10.0^^(-16)
                 , rho ppi == rho (rotateOfZ ppi 1.0) 
                 , rho ppi == rho (rotateOfX ppi 2.3)
                 , rho ppi == rho (rotateOfY ppi 3.4)
                 , (ppiboostedback `veq` ppi) precision
                 , (ppirotback `veq` ppi) precision
                 ]
    let enumeratedList = zip testli [1..length testli]
    print (map (\ (x,y) -> if x then show y ++ ": Ok."
                          else show y ++ ". Fail.") enumeratedList)

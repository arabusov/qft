import Lorentz
import Data.Complex

main = do
    let a = HermVector (Vector (2.0:+0.0) (3.0:+0.0) (4.0:+5.0))
    let zeroV = HermVector (Vector (0.0:+0.0) (0.0:+0.0) (0.0:+0.0))
    putStr "1: "
    if (a `cdot` a == 54.0:+0)
        then do print "Ok."
        else do print "Fail."
    putStr "2: "
    if (a `cdot` zeroV == 0.0:+0.0)
        then do print "Ok."
        else do print "Fail."
    putStr "3: "
    if ( (0.0:+0.0) `vmult` a == zeroV)
        then do print "Ok."
        else do print "Fail."
    putStr "4: "
    if ( a `vplus` zeroV == a)
        then do print "Ok."
        else do print "Fail."
    let mpi = 0.139
    let pv = Vector 0.3 0.2 0.5
    let ppi = toFourVector mpi pv
    putStr "5: "
    if magnitude((ppi `cdot` ppi) - (mpi*mpi :+ 0)) < 10.0^^(-16)
        then do print "Ok."
        else do 
            putStr "Fail. ppi**2 - mpi**2 = "
            print $ ppi `cdot` ppi - (mpi*mpi :+ 0)


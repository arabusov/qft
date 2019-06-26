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


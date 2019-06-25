import Lorentz
import Data.Complex

main = do
    let a = Vector (2.0:+0.0) (3.0:+0.0) (4.0:+5.0)
    let zeroV = Vector (0.0:+0.0) (0.0:+0.0) (0.0:+0.0)
    putStr "1: "
    if (a `scalarMult` a == 54.0:+0)
        then do print "Ok."
        else do print "Fail."
    putStr "2: "
    if (a `scalarMult` zeroV == 0.0:+0.0)
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


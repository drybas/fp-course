module Demo where
import Control.Monad (replicateM)

newtype State s a = State { runState ::s -> (a, s)}

instance Monad (State s) where 
    return a = State $ \ st-> (a, st)
    m >>= k = State $ \st ->
        let (a,st') = runState m st
            m' = k a
        in runState m' st'

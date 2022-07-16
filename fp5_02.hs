{-

f :: a -> Maybe b            -- m = Maybe
f :: a -> [] b               -- m = []
f :: a -> (Either s) b       -- m = Either s
f :: a -> ((,) s) b          -- m = ((,) s)
f :: a -> ((->) e) b         -- m = ((->) e)
f :: a -> (State s) b        -- m = State s
f :: a -> IO b               -- m = IO

f :: a -> m b

-}

import Control.Monad


data Log a = Log [String] a deriving Show

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = \x -> Log ([msg]) . f $ x 

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = let (Log z r1) = f x
                        (Log y r2) = g r1 
                    in Log (z ++ y) r2 

returnLog :: a -> Log a
returnLog a = Log [] a

add1Log = toLogger (+1) "added one"
mul3Log = toLogger (*3) "multiple three"


-- Execution 
-- Log ["nothing done yet"] 0 `bindLog` add1Log

{-- three laws enforced --}
bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log x a) f = let (Log z r) = f a
              in Log (x ++ z) r 


{-- two laws violated --}

{-- bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log x a) f = let (Log z r) = f a
              in Log (z ++ x ++ z) r 
--}

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

instance Functor Log where
  fmap = liftM

instance Applicative Log where
  pure = return
  (<*>) = ap

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList a xs = foldl (>>=) (return a) xs
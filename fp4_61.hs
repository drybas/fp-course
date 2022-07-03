import Data.Map
import Data.Monoid
--import Prelude (Semigroup, Show, Eq, Maybe, Bool)

--type Endo a = a -> a
--type EndoInt = Endo (Endo Int)

{-- (Int->Int) -> Int->Int --}
{-- (a->a) -> (a->a) -> Int --}

{-- type A = Int -> Int -> (Int -> Int)
type B = (Int -> Int) -> (Int -> Int)
type C = Int -> (Int -> Int)
type D = Int -> Int
type E = Int -> Int -> Int -> Int
type F = (Int -> Int) -> Int
type G = (Int -> Int) -> Int -> Int
type H = Int
type J = Int -> Int ->Int --}

{-- newtype A = A a - 
newtype A = A A A   -
newtype A a b = A a b -
newtype A a b = A a  +
newtype A a = A a a  -
newtype A a b = A b  +
newtype A = A A      +
newtype A = A -
newtype A a = A a +
newtype A a = A --}

newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Semigroup Xor where
    (Xor x) <> (Xor y) = Xor (x /= y)

instance Monoid Xor where
    mempty = Xor False
    mappend (Xor x) (Xor y) = Xor (x /= y)

-----------------------------------------------------

newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Semigroup (Maybe' a) where 
    a <> b = undefined 

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' (Just mempty)
    (Maybe' (Just a)) `mappend` (Maybe' (Just b)) = Maybe' (Just (a `mappend` b))
    _ `mappend` _ = Maybe' Nothing 

import Control.Monad.State (Functor)

data Point3D a = Point3D a a a deriving Show

instance Functor Point3D where
    fmap f (Point3D x y z)= Point3D (f x) (f y) (f z) 

data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)


instance Functor GeomPrimitive where
    fmap f (Point p) = Point (fmap f p)
    fmap f (LineSegment a b) = LineSegment (fmap f a)(fmap f b)

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

instance Functor Tree where
    fmap f (Leaf(Just a)) = Leaf (Just $ f a)
    fmap f (Leaf Nothing) = Leaf Nothing
    fmap f (Branch l (Just a) r) = Branch (fmap f l) (Just $ f a) (fmap f r) 
    fmap f (Branch l Nothing  r) = Branch (fmap f l) Nothing (fmap f r) 
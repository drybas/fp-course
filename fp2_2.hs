
class Printable a where 
    toString :: a -> String

instance Printable Bool where 
    toString True = "true"
    toString False = "false"

instance Printable () where
    toString () = "unit type"

instance (Printable a, Printable b) => Printable (a,b) where 
    toString p = "(" ++ toString (fst p) ++ "," ++ toString (snd p) ++ ")"


class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab x | mork == True && gork == True = stomp $ stab x
                  | gork == True = stab x
                  | mork == True = stomp x
                  | otherwise = x
            where 
                    gork = doesEnrageGork x
                    mork = doesEnrageMork x


{-- class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a = 
        let 
            stompStab (True, True) = stomp . stab
            stompStab (True, _   ) = stomp
            stompStab (_   , True) = stab
            stompStab _            = id
        in stompStab (doesEnrageMork a, doesEnrageGork a) a
        --}

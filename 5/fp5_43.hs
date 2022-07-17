pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x = do 
    let t = (x `div` 2)
    m <- [1..t]
    n <- [1..m]

    let a = m * m - n * n
    let b = 2 * m * n 
    let c = m * m + n * n

    if a > 0 && b > 0 && c > 0 && c <= x then [(a,b,c)] else []
            
pythagoreanTriple' :: Int -> [(Int, Int, Int)]
pythagoreanTriple' x = [(a, b, c) | a <- [1..x], b <- [a..x], c <- [b..x], a * a + b * b == c * c ]

pythagoreanTriple'' :: Int -> [(Int, Int, Int)]
pythagoreanTriple'' x = do
        a <- [1..x]
        b <- [a..x]
        c <- [b..x]
        True <- [a * a + b * b == c * c] 
        return (a, b, c)

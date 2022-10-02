type Board = [[Int]] 

nextPositions :: Board -> [Board]
nextPositions x = [[(t:xs)] | xs <- x, t <- [1, 2, 3]]

nextPositionsN :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN b n pred | n < 0  = []
                        | n == 0 = filter pred [b]
                        | n > 0 = do  
                            p <- nextPositions b
                            nextPositionsN p (n - 1) pred
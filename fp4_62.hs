import Prelude hiding (lookup)

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance MapLike ListMap where
    empty = ListMap []
    lookup k (ListMap []) = Nothing
    lookup k (ListMap ((k', v):xs)) | k' == k = Just v
                                    | otherwise = lookup k (ListMap xs) 

    --insert k v (ListMap []) = ListMap[(k,v)]
    insert k v (ListMap []) = ListMap[(k,v)]
    insert k v (ListMap ((k', v'):xs)) | k == k' = ListMap ((k,v): xs)
                                       | otherwise = ListMap ((k', v'):getListMap (insert k v (ListMap xs))) 
    --insert k v (ListMap ((k', v'):xs)) | k' == k = (ListMap [(k,v)])
    --                                   | otherwise = ListMap ((k', v') : rs) 
    --                                        where rs = getListMap (insert k v (ListMap xs))
                                            --where rs = getListMap(insert k v (ListMap xs))

    delete k (ListMap []) = ListMap []
    delete k (ListMap ((k',v):xs)) | k' == k = ListMap xs
                                   | otherwise = ListMap ((k', v) : getListMap (delete k (ListMap xs)))
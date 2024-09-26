
seqMaybe :: [] (Maybe a) -> Maybe ([] a)
seqMaybe [] = return []
seqMaybe (mx:mxs) = do
    x <- mx
    xs <- seqMaybe mxs
    return (x:xs)  

-- Function to return list of all suffixes of a list
suffixList :: [a] -> [[a]]
suffixList [] = [[]]
suffixList l@(_:xs) = l:suffixList xs

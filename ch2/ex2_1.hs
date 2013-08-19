import System.Environment

-- Function to return list of all suffixes of a list
-- This is O(n^2) as confirmed by profiling

suffixList :: [a] -> [[a]]
suffixList [] = [[]]
suffixList l@(_:xs) = l:suffixList xs

main = do
  args <- getArgs
  if length args /= 1
    then putStrLn "One argument required."
    else do
      let l = suffixList [1..n] where n = read $ head args :: Int
      putStrLn $ show l
      putStrLn "Suffix List Calculated."

import Criterion.Main

-- Function to return list of all suffixes of a list
-- This function is O(n), confirmed by profiling
suffixList :: [Int] -> [[Int]]
suffixList [] = [[]]
suffixList l@(_:xs) = l:suffixList xs

main = defaultMain [
        bgroup "suffixList" [
          bench "suffixList [1..100]" $ nf suffixList [1..100]
        , bench "suffixList [1..1000]" $ nf suffixList [1..1000]
        ]
      ]

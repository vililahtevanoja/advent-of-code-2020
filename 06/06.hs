
import           Data.List (intersect, nub)

isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _          = True
isPrefixOf _ []          = False
isPrefixOf (x:xs) (y:ys) = (x == y) && xs `isPrefixOf` ys

splitAtSubSeq :: (Eq a) => [a] -> [a] -> [[a]]
splitAtSubSeq _ [] = []
splitAtSubSeq sub seq = splitAtSubSeq' sub seq [] []
  where
    splitAtSubSeq' _ [] subacc acc = reverse $ reverse subacc:acc
    splitAtSubSeq' sub (x:xs) subacc acc
      | sub `isPrefixOf` (x:xs) = splitAtSubSeq' sub (drop (length sub - 1) xs) [] (reverse subacc:acc)
      | otherwise               = splitAtSubSeq' sub xs (x:subacc) acc

splitAtExclusive :: (Eq a) => a -> [a] -> [[a]]
splitAtExclusive _ [] = []
splitAtExclusive el seq = splitAtExclusive' el seq [] []
  where
    splitAtExclusive' _ [] subacc acc = reverse $ reverse subacc:acc
    splitAtExclusive' el (x:xs) subacc acc
      | el == x    = splitAtExclusive' el xs [] (reverse subacc:acc)
      | otherwise = splitAtExclusive' el xs (x:subacc) acc

solve1 :: String -> Int
solve1 inputs = do
  let groups =  map (filter ('\n' /= )) $ splitAtSubSeq "\n\n" inputs
  let nubbed = map nub groups
  let questionsAnsweredPerGroup = map length nubbed
  sum questionsAnsweredPerGroup

solve2 :: String -> Int
solve2 inputs = do
  let groups = map (splitAtExclusive '\n') $ splitAtSubSeq "\n\n" inputs
  let groupIntersections = map (foldl1 intersect) groups
  sum $ map length groupIntersections

main :: IO ()
main = do
  inputs <- readFile "input.txt"
  let p1 = solve1 inputs
  putStrLn $ "Part 1: " ++ show p1
  let p2 = solve2 inputs
  putStrLn $ "Part 2: " ++ show p2

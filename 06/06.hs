
import           Data.List (intersect, union)

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
splitAtExclusive el = splitAtSubSeq [el]

solve1 :: String -> Int
solve1 inputs = do
  let groups =  map (splitAtExclusive '\n') $ splitAtSubSeq "\n\n" inputs
  let groupUnions = map (foldl1 union) groups
  sum $ map length groupUnions

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

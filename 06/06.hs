
import           Data.List (intersect, nub)

isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _              = True
isPrefixOf _ []              = False
isPrefixOf (c1:cs1) (c2:cs2) = (c1 == c2) && cs1 `isPrefixOf` cs2

splitAtSeq :: (Eq a) => [a] -> [a] -> [[a]]
splitAtSeq _ [] = []
splitAtSeq sub str = splitAtSeq' sub str [] []
  where
    splitAtSeq' _ [] subacc acc = reverse $ reverse subacc:acc
    splitAtSeq' sub (c:cs) subacc acc
      | sub `isPrefixOf` (c:cs) = splitAtSeq' sub (drop (length sub) (c:cs)) [] (reverse subacc:acc)
      | otherwise               = splitAtSeq' sub cs (c:subacc) acc

splitAtExclusive :: (Eq a) => a -> [a] -> [[a]]
splitAtExclusive _ [] = []
splitAtExclusive el seq = splitAtExclusive' el seq [] []
  where
    splitAtExclusive' _ [] subacc acc = reverse $ reverse subacc:acc
    splitAtExclusive' el (c:cs) subacc acc
      | el == c    = splitAtExclusive' el cs [] (reverse subacc:acc)
      | otherwise = splitAtExclusive' el cs (c:subacc) acc

solve1 :: String -> Int
solve1 inputs = do
  let groups =  map (filter ('\n' /= )) $ splitAtSeq "\n\n" inputs
  let nubbed = map nub groups
  let questionsAnsweredPerGroup = map length nubbed
  sum questionsAnsweredPerGroup

solve2 :: String -> Int
solve2 inputs = do
  let groups = map (splitAtExclusive '\n') $ splitAtSeq "\n\n" inputs
  let groupIntersections = map (foldl1 intersect) groups
  sum $ map length groupIntersections

main :: IO ()
main = do
  inputs <- readFile "input.txt"
  let p1 = solve1 inputs
  putStrLn $ "Part 1: " ++ show p1
  let p2 = solve2 inputs
  putStrLn $ "Part 2: " ++ show p2

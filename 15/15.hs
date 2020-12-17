import           Data.List (maximumBy)
import qualified Data.Map  as M
import           Data.Ord  (comparing)

isFirstTimeSpoken :: Int -> [(Int, Int)] -> Bool
isFirstTimeSpoken n ns = not (any (\(i, nn) -> nn == n) ns)

lastSpokenDelta :: Int -> [(Int, Int)] -> Int
lastSpokenDelta n ns =
  let
    spokenRounds = reverse (filter (\(_,x) -> x == n) ns)
    first = fst (head spokenRounds)
    second = fst (head (tail spokenRounds))
  in first - second


playGame :: Int -> [(Int,Int)] -> [(Int, Int)]
playGame rounds spoken = go True (fst (last spoken) + 1) (snd (last spoken)) spoken
  where
    go :: Bool -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
    go lastWasFirstSpoken round lastN ns
      | round > rounds = ns
      | lastWasFirstSpoken = go False (round+1) 0 (ns ++ [(round, 0)])
      | otherwise =
        let
          nextN = lastSpokenDelta lastN ns
          firstTime = isFirstTimeSpoken nextN ns
        in go firstTime (round+1) nextN (ns ++ [(round, nextN)])

isFirstTimeSpokenMap :: Int -> M.Map Int [Int] -> Bool
isFirstTimeSpokenMap = M.notMember

lastSpokenDeltaMap :: Int -> M.Map Int [Int] -> Int
lastSpokenDeltaMap n nmap = case M.lookup n nmap of
  Just is -> head reversed - head (tail reversed)
    where reversed = reverse is
  Nothing -> error ("wtf: " ++ show n)

insertSpoken :: Int -> Int -> M.Map Int [Int] -> M.Map Int [Int]
insertSpoken n round ns = case M.lookup n ns of
  Just rs -> M.insert n (last rs : [round]) ns
  Nothing -> M.insert n [round] ns

playGameAlt :: Int -> M.Map Int [Int] -> M.Map Int [Int]
playGameAlt rounds spoken = go lastWasUnique (roundsSoFar+1) lastN spoken
  where
    lastWasUnique = 1 == length (snd $ maximumBy (comparing (\(n, is) -> length is)) $ M.assocs spoken)
    roundsSoFar = sum $ map length $ M.elems spoken
    lastN = fst $  maximumBy (comparing (\(n,is) -> maximum is)) $ M.assocs spoken
    go :: Bool -> Int -> Int -> M.Map Int [Int] -> M.Map Int [Int]
    go lastWasFirstSpoken round lastN ns
      | round > rounds = ns
      | lastWasFirstSpoken = go False (round+1) 0 (insertSpoken 0 round ns)
      | otherwise =
        let
          nextN = lastSpokenDeltaMap lastN ns
          firstTime = isFirstTimeSpokenMap nextN ns
        in go firstTime (round+1) nextN (insertSpoken nextN round ns)


main :: IO ()
main = do
  let exampleResult = playGame 10 (zip [1..] [0,3,6])
  let input = [9,6,0,10,18,2,1]
  let inputData = M.fromList $ zipWith (\ i n -> (n, [i])) [1..] input
  let untilRoundP1 = 2020
  let p1Game = playGameAlt untilRoundP1 inputData
  putStrLn $ "Part 1: " ++ show (fst $ head $ filter (\(n, is) -> untilRoundP1 `elem` is) $ M.assocs p1Game)

  let inputDataP2 = M.fromList $ zipWith (\ i n -> (n, [i])) [1..] input
  let untilRoundP2 = 30000000
  let p2Game = playGameAlt untilRoundP2 inputDataP2
  let p2Result = fst $ head $ filter (\(n, is) -> untilRoundP2 `elem` is) $ M.assocs p2Game
  putStrLn $ "Part 2: " ++ show p2Result

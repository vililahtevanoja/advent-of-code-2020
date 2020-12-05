data Seat = Seat {row::Int, column::Int, seatId::Int } deriving (Show)

seatBinSearch :: String -> Int
seatBinSearch [] = 0
seatBinSearch (c:cs) = inner c cs
  where
    inner 'F' "" = 0
    inner 'L' "" = 0
    inner 'B' "" = 1
    inner 'R' "" = 1
    inner 'F' (c:cs) = 0 + inner c cs
    inner 'L' (c:cs) = 0 + inner c cs
    inner 'B' (c:cs) = (2 ^ length (c:cs)) + inner c cs
    inner 'R' (c:cs) = (2 ^ length (c:cs)) + inner c cs
    inner _ _ = 0

parseSeat :: String -> Seat
parseSeat s =
  let row = seatBinSearch (take 7 s)
      col = seatBinSearch (take 3 (drop 7 s))
  in Seat row col ((row*8) + col)

main :: IO ()
main = do
  inputs <- lines <$> readFile "input.txt"
  let seats = map parseSeat inputs
  print $ "Part 1: " ++ show (maximum (map seatId seats))
  let existingIds = map seatId seats
  let missingSeats = filter (`notElem` existingIds) [1..920]
  let mySeat = filter (\id -> id-1 `elem` existingIds && id+1 `elem` existingIds) missingSeats
  print $ "Part 2: " ++ show (head mySeat)

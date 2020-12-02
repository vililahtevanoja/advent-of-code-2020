main :: IO ()
main = do
    inputs <- lines <$> readFile "input.txt"
    let ns = map read inputs :: [Int]
    let result = head [a*b*c | a <- ns, b <- ns, c <- ns, a+b+c == 2020]
    print result

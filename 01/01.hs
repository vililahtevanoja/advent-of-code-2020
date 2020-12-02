main :: IO ()
main = do
    inputs <- lines <$> readFile "input.txt"
    let ns = map read inputs :: [Int]
    let result = head [a*b | a <- ns, b <- ns, a+b == 2020]
    print result

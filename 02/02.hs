import Text.ParserCombinators.ReadP
import Data.Char

data PwInfo = PwInfo {
    constraint1 :: Int,
    constraint2 :: Int,
    letter :: Char,
    pw :: String
} deriving (Show)

countCharacters :: String -> Char -> Int
countCharacters s c = foldl (\count char -> if char == c then count+1 else count) 0 s

digit :: ReadP Char
digit = satisfy isDigit

alphaNum :: ReadP Char
alphaNum = satisfy isAlphaNum

lineToPwInfo :: ReadP PwInfo
lineToPwInfo = do
    constraint1 <- many digit
    _ <- string "-"
    constraint2 <- many digit
    _ <- string " "
    letter <- count 1 alphaNum
    _ <- string ": "
    pw <- many alphaNum
    return (PwInfo (read constraint1 :: Int) (read constraint2 :: Int) (head letter) pw)

isCompliantPart1 :: PwInfo -> Bool
isCompliantPart1 pwi = do
    let c1 = constraint1 pwi
    let c2 = constraint2 pwi
    let letterCount = countCharacters (pw pwi) (letter pwi)
    letterCount >= c1 && letterCount <= c2

isCompliantPart2 :: PwInfo -> Bool
isCompliantPart2 pwi = do
    let c1 = constraint1 pwi - 1
    let c2 = constraint2 pwi - 1
    let c1Fulfilled = pw pwi!!c1 == letter pwi
    let c2Fulfilled = pw pwi!!c2 == letter pwi
    (c1Fulfilled || c2Fulfilled) && not (c1Fulfilled && c2Fulfilled)


main :: IO ()
main = do
    inputs <- lines <$> readFile "input.txt"
    let pwis = map ((fst . last) . readP_to_S lineToPwInfo) inputs
    let compliantsPart1 = filter isCompliantPart1 pwis
    let compliantsPart2 = filter isCompliantPart2 pwis
    putStrLn $ "Part 1: Compliant password count: " ++ show (length compliantsPart1)
    putStrLn $ "Part 2: Compliant password count: " ++ show (length compliantsPart2)

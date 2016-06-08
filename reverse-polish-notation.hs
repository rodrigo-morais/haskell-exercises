import Data.Char

calculate :: String -> Int
calculate [] = 0
calculate (x:[]) = digitToInt x
calculate (x:xs)
  | isDigit x = calculate $ xs ++ [x]
  | '+' == x = calculate $ (dropLastTwo xs) ++ [sumLastTwo xs]
  | '*' == x = calculate $ (dropLastTwo xs) ++ [timesLastTwo xs]
  | otherwise = 0

sumLastTwo :: String -> Char
sumLastTwo xs =
  intToDigit $
  (+) (takeFirst xs) (takeSecond xs)

timesLastTwo :: String -> Char
timesLastTwo xs =
  intToDigit $
  (*) (takeFirst xs) (takeSecond xs)

dropLastTwo :: String -> String
dropLastTwo xs = reverse $ drop 2 $ reverse xs

takeFirst :: String -> Int
takeFirst xs = digitToInt $ head $ take 1 $ reverse xs

takeSecond :: String -> Int
takeSecond xs = digitToInt $ head $ reverse $ take 2 $ reverse xs

import Data.Char

calculate :: [String] -> Double
calculate [] = 0
calculate (x:[]) = read x
calculate (x:xs)
  | isValidNumber x = calculate $ xs ++ [x]
  | "+" == x = calculate $ (dropLastTwo xs) ++ [sumLastTwo xs]
  | "-" == x = calculate $ (dropLastTwo xs) ++ [subtractionLastTwo xs]
  | "*" == x = calculate $ (dropLastTwo xs) ++ [timesLastTwo xs]
  | "/" == x = calculate $ (dropLastTwo xs) ++ [divisionLastTwo xs]
  | otherwise = 0

sumLastTwo :: [String] -> String
sumLastTwo xs =
  show $
  (+) (takeSecond xs) (takeFirst xs)

subtractionLastTwo :: [String] -> String
subtractionLastTwo xs =
  show $
  (-) (takeSecond xs) (takeFirst xs)

timesLastTwo :: [String] -> String
timesLastTwo xs =
  show $
  (*) (takeSecond xs) (takeFirst xs)

divisionLastTwo :: [String] -> String
divisionLastTwo xs =
  show $
  (/) (takeFirst xs) (takeSecond xs)

dropLastTwo :: [String] -> [String]
dropLastTwo xs = reverse $ drop 2 $ reverse xs

takeFirst :: [String] -> Double
takeFirst xs = read $ head $ take 1 $ reverse xs

takeSecond :: [String] -> Double
takeSecond xs = read $ head $ reverse $ take 2 $ reverse xs

isValidNumber :: String -> Bool
isValidNumber [] = False
isValidNumber (x:[]) = if isDigit x then True else False
isValidNumber (x:xs)
  | isDigit x = True && isValidNumber xs
  | otherwise = False 
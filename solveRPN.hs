solveRPN :: String -> Double
solveRPN = head . foldl foldingFunction [] . words
  where foldingFunction (x:y:ys) "*" = (x * y) : ys
        foldingFunction (x:y:ys) "+" = (x + y) : ys
        foldingFunction (x:y:ys) "-" = (x - y) : ys
        foldingFunction xs numberString = read numberString : xs
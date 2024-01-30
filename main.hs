-- Function to calculate the square root of an integer
isqrt :: Integral a => a -> a
isqrt = floor . sqrt . fromIntegral

-- Function to check if a number is prime
isPrime :: Integral a => a -> Bool
isPrime k = if k > 1 then null [ x | x <- [2..isqrt k], k `mod` x == 0] else False

-- Main function
main :: IO ()
main = do
   putStrLn "Please enter a number:"
   input <- getLine
   let num = read input :: Int
   let isPrimeNum = isPrime num
   if isPrimeNum
      then putStrLn (show num ++ " is a prime number.")
      else putStrLn (show num ++ " is not a prime number.")

import System.Random

threeNumbers :: StdGen -> String
threeNumbers gen = 
    let (firstNumber, newGen) = random gen :: (Int, StdGen)
        (secondNumber, newGen') = random newGen :: (Int, StdGen)
        (thirdNumber, _) = random newGen' :: (Int, StdGen)
    in unlines [show firstNumber, show secondNumber, show thirdNumber]

main = do
    number <- getLine
    putStr $ threeNumbers (mkStdGen $ read number)

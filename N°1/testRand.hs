import Data.Char
import System.Random

getFromStdin :: String -> (IO a) -> (a -> Bool) -> (a -> b) -> IO b
getFromStdin promptAgain inputF isOk transformOk = do
  input <- inputF
  if isOk input
     then return $ transformOk input
     else do
       putStr promptAgain
       getFromStdin promptAgain inputF isOk transformOk

getYN :: String -> IO Char
getYN promptAgain = 
  getFromStdin promptAgain getChar (`elem` "yYnN") toUpper

otherGen :: IO Bool
otherGen = do 
  putStr "Other Gen ? "
  again <- getYN "\nOther Gen ? "
  return $ again == 'Y' 

displayInt :: Int -> IO ()
displayInt x = putStrLn $ show x

displayStdGen :: StdGen -> IO ()
displayStdGen x = putStrLn $ show x

displayListOfTuples :: [Position] -> IO()
displayListOfTuples zs = sequence_ [putStrLn ("(" ++ show a ++ ", " ++ show b ++ ")") | (a,b) <- zs]
import System.Environment

import P03

main :: IO ()
main = do
 argv <- getArgs
 putStrLn $ show $ solve $ (read (argv !! 0) :: Int)
 return ()

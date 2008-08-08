import System.Environment
 
{- Haskell warmups -}
main1 :: IO ()
main1 = do 
  args <- getArgs
  putStrLn ("Hello, " ++ args !! 0)

main2 :: IO ()
main2 = do 
  args <- getArgs
  putStrLn ("Hello, "
                ++ args !! 0
                ++ (show (1 + (read (args !! 1)))))

main3 :: IO ()
main3 = do 
  putStrLn "Name? "
  name <- getLine
  putStrLn ("Hello, " ++ name)

main4 :: IO ()
main4 = return ()




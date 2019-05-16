module Main where

import           Control.Concurrent  (forkIO, threadDelay)
import           Control.Monad.Trans (lift)
import           Task5               (allocate, resourceFork, runAllocateT)

main :: IO ()
main = 
  runAllocateT $ do
    (_, _) <- allocate (putStrLn "A aquired") (\_ -> putStrLn "A released")
    lift $ putStrLn "After a"
    resourceFork
      (\action -> () <$ forkIO action)
      (allocate (putStrLn "C aquired") (\_ -> putStrLn "C released") >>
       (lift $ threadDelay 5000000) >>
       --(error "BLYAAAAA") >>
       lift (putStrLn "Finished helper thread"))
    --lift $ threadDelay 500000
    --_ <- error "AAAAAAAAAAA"
    lift $ putStrLn "finishing"

module Main where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Exception  (uninterruptibleMask_)
import Task5 (runAllocateT, allocate, resourceFork)
import Control.Monad.Trans (lift)

--import           Control.Monad.Trans (lift)
-- import qualified Task1         as T1
--import qualified Task5               as T5
myFork :: IO () -> IO ()
myFork act =
  uninterruptibleMask_ $ do
    _ <-
      forkIO $ uninterruptibleMask_ $ do
        threadDelay 1000000
        act
    return ()

main :: IO ()
main =
  runAllocateT $ do
    (_, _) <- allocate (putStrLn "A aquired") (\_ -> putStrLn "A released")
    lift $ putStrLn "After a"
    resourceFork
      (\action -> () <$ forkIO action)
      (allocate (putStrLn "C aquired") (\_ -> putStrLn "C released") >> lift (putStrLn "Finished helper thread"))
    --lift $ threadDelay 1000000
    lift $ putStrLn "finishing"
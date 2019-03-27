module Kernel 
  ( Cont (..)
  , cont
  , exit
  , fork
  , kernel
  , main1
  , main2
  , main3
  , readLine
  , runProcess
  , writeLine
  , yield
  ) where

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

cont :: ((a -> r) -> r) -> Cont r a
cont f = Cont { runCont = f }

instance Functor (Cont r) where
  fmap f cnt = cont $ \callback -> runCont cnt (callback . f)

instance Applicative (Cont r) where
  pure a = cont $ \callback -> callback a

  cntAB <*> cntA = cont $ \callback -> runCont cntAB (\f -> runCont cntA (callback . f))

instance Monad (Cont r) where
  return = pure

  cnt >>= f = cont $ \callback -> runCont cnt (\a -> runCont (f a) callback)

data Status = Success | Failure
data ProcessType = Child | Parent

data SysCall 
  = Exit Status
  | Read (String -> SysCall)
  | Write String (() -> SysCall)
  | Fork (ProcessType -> SysCall)
  | Yield (() -> SysCall)

exit :: Status -> Cont SysCall ()
exit status = cont $ \_ -> Exit status

readLine :: Cont SysCall String
readLine = cont Read

writeLine :: String -> Cont SysCall ()
writeLine s = cont $ Write s

fork :: Cont SysCall ProcessType
fork = cont Fork

yield :: Cont SysCall ()
yield = cont Yield

kernel :: [SysCall] -> IO ()
kernel [] = return ()
kernel (Exit Success:xs) = kernel xs
kernel (Exit Failure:xs) = do
  putStrLn "Procces failed"
  kernel xs
kernel (Write s cnt:xs) = do
  putStrLn s
  kernel $ cnt ():xs 
kernel (Read cnt:xs) = do
  s <- getLine
  kernel $ cnt s:xs
kernel (Fork cnt:xs) = kernel $ [cnt Child, cnt Parent] ++ xs
kernel (Yield cnt:xs) = kernel $ xs ++ [cnt ()] 

runProcess :: Cont SysCall () -> IO ()
runProcess p = kernel [runCont p (const $ Exit Success)]

main1 :: Cont SysCall ()
main1 = do
  x <- readLine
  let str = "Hello, " ++ show x ++ "!"
  writeLine str
  exit Success

main2 :: Cont SysCall ()
main2 = do
  x <- readLine
  writeLine $ "I am " ++ x
  who <- fork
  case who of
    Parent -> do 
      writeLine "I am parent"
      exit Success
    Child -> do
      writeLine "I am child"
      yield
      exit Failure

main3 :: Cont SysCall ()
main3 = do
  writeLine "I forgot to exit"
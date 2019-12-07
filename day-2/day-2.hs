module Main where

main :: IO ()
main = do input <- getInput
          let (noun,verb) = findResult 19690720 input
          print $ 100 * noun + verb

findResult :: Int -> [Int] -> (Int,Int)
findResult target mem = fst . head . filter ((==target) . snd) . map result $ ixs
  where ixs = (,) <$> [0 .. 99] <*> [0 .. 99]
        result x@(noun,verb) = (x,snd $ runMachine (patch noun verb >> exec >> readIx 0) mem)

-- Patch memory with noun and verb
patch :: Int -> Int -> Turing ()
patch noun verb = writeT 1 noun >> writeT 2 verb

-- IntCode Machine
type Machine = ([Int],Int)

data Turing a = Turing { unTuring :: Machine -> (Machine,a)  }

runMachine :: Turing a -> [Int] -> (Machine,a)
runMachine t xs = (unTuring t) (xs,0)

instance Functor Turing where
  fmap f (Turing ut) = Turing (fmap f . ut)

instance Applicative Turing where
  pure a = Turing $ \tape -> (tape,a)
  Turing utf <*> Turing uta = Turing $
                              \s -> let (s',f) = utf s
                                        (s'',a) = uta s'
                                    in (s'', f a)

instance Monad Turing where
  return = pure
  Turing uta >>= f = Turing $
                     \s -> let (s',a) = uta s
                               (Turing utb) = f a
                           in utb s'
progC :: Turing Int
progC = Turing $ \(tape,pc) -> ((tape,pc),pc)

movePC :: Turing ()
movePC = Turing $ \(tape,pc) -> ((tape, if pc + 1 >= length tape then -1 else pc + 1),())

writeT :: Int -> Int -> Turing ()
writeT x v = write' x v
  where write' x v = Turing $ \(tape,pc) -> ((update x (const v) tape, pc), ())

readT :: Turing Int
readT = read' >>= \a -> movePC >> return a
  where read' = Turing $ \(tape,pc) -> ((tape,pc), tape !! pc)

readIx :: Int -> Turing Int
readIx ix = Turing $ \(tape,pc) -> ((tape,pc), tape !! ix)

haltT :: Turing ()
haltT = Turing $ \(tape,pc) -> ((tape,-1),())

exec :: Turing ()
exec = do pc <- progC
          if pc < 0
            then return ()
            else execOp >> exec

execOp :: Turing ()
execOp = do op <- readT
            case op of
              1 -> execAdd
              2 -> execMult
              99 -> haltT
              _ -> haltT
              where execAdd = execNumOp (+)
                    execMult = execNumOp (*)
                    execNumOp f = do (ix1,ix2,to) <- (,,) <$> readT <*> readT <*> readT
                                     (rand1,rand2) <- (,) <$> readIx ix1 <*> readIx ix2
                                     writeT to $ rand1 `f` rand2
-- Utility
getInput :: IO [Int]
getInput = readFile "input.txt" >>= pure . map toInt . words . map commaSpace
  where toInt = read
        commaSpace ',' = ' '
        commaSpace c = c

update :: Int -> (a -> a) -> [a] -> [a]
update ix f = map g . zip [0 ..]
  where g (i,a) = if i == ix then f a else a

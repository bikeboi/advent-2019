{-# LANGUAGE TemplateHaskell #-}

module IntCode where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.IO.Class (liftIO)
import Lens.Micro.Platform
import Data.Char (digitToInt)

type Input = [Int]
type Output = [Int]

type IntCode = ExceptT String (WriterT Output (StateT Machine IO))

data Mode = POS | IMM deriving (Eq, Show)
data OpInfo = OpInfo (Mode,Mode,Mode) Int deriving (Eq, Show)

data Machine = Machine { _ic_PC :: Int
                       , _ic_MEM :: [Int]
                       , _ic_IC :: Int } deriving (Eq, Show)
makeLenses ''Machine

initialize :: [Int] -> Machine
initialize mem = Machine 0 mem 0

runIC :: Machine -> IntCode a -> IO ((Either String a, [Int]), Machine)
runIC mach ic = runStateT (runWriterT (runExceptT ic)) mach

testIC :: IntCode a -> IO ((Either String a, [Int]), Machine)
testIC = runIC (Machine 0 (reverse [0 .. 100]) 0)

-- Execute all operations
execute :: IntCode ()
execute = do pc <- use ic_PC
             if pc < 0
               then return ()
               else execOp >> execute
--

-- OpCodes
execOp :: IntCode ()
execOp = do OpInfo modes@(m3,m2,m1) code <- decodeOp <$> readPC
            case code of
              1 -> execAdd m1 m2
              2 -> execMult m1 m2
              3 -> execReadIn m1
              4 -> execWriteOut m1
              5 -> execJIT m1 m2
              6 -> execJIF m1 m2
              7 -> execLT m1 m2
              8 -> execEQ m1 m2
              99 -> execHalt
              _ -> tell [code] >> throwError ("Unsupported operation: " ++ show code)

-- Other
execHalt :: IntCode ()
execHalt = ic_PC .= (-1)

--

-- Branching
execJIT, execJIF :: Mode -> Mode -> IntCode()
execJIT = jumpCond (/= 0)
execJIF = jumpCond (== 0)

jumpCond :: (Int -> Bool) -> Mode -> Mode -> IntCode ()
jumpCond pred m1 m2 = do (cond,target) <- (,) <$> arg m1 <*> arg m2
                         if pred cond then ic_PC .= target else return ()
--


-- Binary Operations
execEQ :: Mode -> Mode -> IntCode ()
execEQ = execBinOp (\x y -> fromEnum $ x == y)

execLT :: Mode -> Mode -> IntCode ()
execLT = execBinOp (\x y -> fromEnum $ x < y)

execAdd :: Mode -> Mode -> IntCode ()
execAdd = execBinOp (+)

execMult :: Mode -> Mode -> IntCode ()
execMult = execBinOp (*)

execBinOp :: (Int -> Int -> Int) -> Mode -> Mode -> IntCode ()
execBinOp f m1 m2 = do (x,y,dest) <- (,,) <$> arg m1 <*> arg m2 <*> arg IMM
                       writeAddr dest (x `f` y)

--

-- IO Operations
execReadIn :: Mode -> IntCode ()
execReadIn m = do dest <- arg IMM
                  readIn >>= writeAddr dest

execWriteOut :: Mode -> IntCode ()
execWriteOut m = do from <- arg m
                    writeOut from

arg :: Mode -> IntCode Int
arg POS = readPC >>= readAddr
arg IMM = readPC

decodeOp :: Int -> OpInfo
decodeOp n = let ns = map digitToInt . show $ n
                 code = if length ns < 5 then replicate (5 - length ns) 0 ++ ns else ns
             in OpInfo <$> decodeMode <*> decodeOp' $ code
  where decodeOp' (_:_:_:rest) = (read :: String -> Int) . concat . map show $ rest
        decodeMode (x:y:z:_) = (,,) (mode x) (mode y) (mode z)
        mode 1 = IMM
        mode 0 = POS

-- Internal Operations
readAddr :: Int -> IntCode Int
readAddr addr = preuse (ic_MEM . ix addr)
                >>= liftMaybe ("Invalid address" ++ show addr)

writeAddr :: Int -> Int -> IntCode ()
writeAddr addr val = ( ic_MEM . ix addr ) .= val

readPC :: IntCode Int
readPC = do res <- use ic_PC >>= readAddr
            incPC
            return res

readIn :: IntCode Int
readIn = do liftIO $ putStr "> "
            liftIO readLn

writeOut :: Int -> IntCode ()
writeOut x = liftIO $ print x

--  Utility
incPC :: IntCode ()
incPC = do pc <- use ic_PC
           mem <- use ic_MEM
           if pc < 0 || pc > length mem
             then return ()
             else ic_PC %= (+1)

liftMaybe :: String -> Maybe a -> IntCode a
liftMaybe s Nothing = throwError s
liftMaybe _ (Just x) = return x

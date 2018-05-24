module Dice (Roll, Result, readroll, resVal, doroll, dolistroll, dorepeatroll) where

import System.Random
import Data.Char
import Data.List

--              4d6+3
data Roll = D Int Int Int 

--              (1, 2) +3
data Result = R [Int] Int

instance Show Roll where
  show (D 1 d 0) = 'd':(show d)
  show (D n d 0) = (show n) ++ "d" ++ (show d)
  show (D n d b) = (show n) ++ "d" ++ (show d) ++ "+" ++ (show b) 
    
instance Show Result where
  show   (R []  0)  = "no result"
  --show (R [x] 0)  = show x
  show r@(R xs  0)  = showlistres xs ++ showResVal r
  show r@(R xs  b)  = showlistres xs ++ " +" ++ show b ++ showResVal r
    --where
showlistres :: [Int] -> String
showlistres xs = "[" ++ (intercalate " + " (map show xs)) ++ "]"

showResVal :: Result -> String
showResVal r = " = " ++ (show.resVal$r)
  
instance Read Roll where
  readsPrec _ input = let 
                        (ns,rest1) = span isDigit (filter (/=' ') input)
                        (dd, rest2) = (take 1 rest1, drop 1 rest1)
                        (ds, rest3) = span isDigit rest2
                        n = if  ns /= [] then read ns :: Int else 1
                        d = if ds /= [] then read ds :: Int else 1
                        in
                        if dd == "d"
                           then case rest3 of
                                     ('+':rest4)     -> let (bs, rest) = span isDigit rest4 
                                                            b = if bs /= [] then read bs :: Int else 0
                                                        in [((D n d b), rest)]
                                     _              -> [((D n d 0), rest3)]
                           else []

readroll:: String -> Roll
readroll string = read string :: Roll

resVal :: Result -> Int
resVal (R [] b) = b
resVal (R xs b) = foldr (+) b xs


doroll :: Roll -> IO (Result)
doroll (D n d b)  | n > 0   = do 
                                res <- dorepeatroll n d
                                return (R res b)
                  | n <= 0  =   return (R []  0)
                  
dosingleroll :: Int -> IO (Int)
dosingleroll d = randomRIO (1, d)


dolistroll :: [Int] -> IO ([Int])
dolistroll []     = return []
dolistroll (r:rs) = do 
                      v    <- dosingleroll r
                      rest <- dolistroll rs
                      return (v:rest)

dorepeatroll ::Int -> Int -> IO ([Int])
dorepeatroll n r = dolistroll (take n (repeat r))
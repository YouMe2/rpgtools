module Main where

import Dice
import Table

import System.IO (BufferMode (NoBuffering, LineBuffering), stdin, stdout, hSetBuffering)

-- STATE
data State = State Bool Tables
--                 quit tabels

startState :: State
startState = State False emptyTables

type Tables = String -> Maybe Table

emptyTables :: Tables
emptyTables _ = Nothing

addTable :: Tables -> String -> Table -> Tables
addTable ts name t n | n == name = Just t
                     | otherwise = ts n

isQuit :: State -> Bool
isQuit (State q _) = q

-- /STATE

-- ACTIONS

type Action = State -> IO State

action :: String -> Action
action str | str == ":quit" || str == ":q"        = quit
           | (take 11 str)  == ":rolltable "      = tablerollaction (drop 11 str)
           | (take 6 str)   == ":roll "           = roll (drop 6 str)
           | (take 4 str)   == ":rt "             = tablerollaction (drop 4 str)
           | (take 2 str)   == ":r"               = roll (drop 2 str)
           | (take 11 str)  == ":loadtable "      = loadtable (drop 11 str)
           | (take 4 str)   == ":lt "             = loadtable (drop 4 str)
           | (take 11 str)  == ":edittable "      = edittable (drop 11 str)
           | (take 4 str)   == ":et "             = edittable (drop 4 str)
           | (take 4 str)    == ":pt "            = printtable (drop 4 str)
           | otherwise                            = roll str

quit :: Action
quit s = return (State True emptyTables)

roll :: String -> Action
roll str s =
  case reads str :: [(Roll, String)] of
       (roll, _):_    -> do
                          res <- doroll roll
                          putStrLn ("Rolling " ++ show roll ++ " :")
                          putStrLn (show res)
                          return s
       _              -> tablerollaction str s

tablerollaction :: String -> Action
tablerollaction str s@(State _ tables) = 
  case tables str of
       Just t   -> do
         
         res <- doroll.tableRoll$t
         let msg = (tableEntry t).resVal$res
         
         putStrLn ("Rolling [" ++ (show.tableRoll$t) ++ "] on \'" ++ (tableName t) ++ "\':")
         putStrLn $ show res
         putStrLn   msg
         return s
       Nothing  -> cmdnotfound str s

loadtable :: String -> Action
loadtable path s@(State _ ts) = do
  file <- readFile path
  let mt = readTable file
  case mt of 
       Just t  -> do 
          putStr "Name the table: "
          tid <- getLine
          return (State False (addTable ts tid t))
       Nothing -> do
          putStrLn "No valid rollable table."
          return s

edittable :: String -> Action
edittable str s@(State _ ts) =
  case ts str of
       Just (T r n tes c) -> do
         putStrLn "Enter roll value(s):"
         rvs <- getLine
         putStrLn "Enter result:"
         res <- getLine
         let ntes = foldr addTEntry tes (entryList [[rvs, res]])
         return (State False (addTable ts str (T r n ntes c)))
       Nothing            -> do
         putStrLn "No valid tablename was found."
         return s

printtable :: String -> Action
printtable str s@(State _ ts) =
  case ts str of
       Just t   -> do
         putStrLn.show$t
         return s
       Nothing  -> do
         putStrLn "No valid tablename was found."
         return s



cmdnotfound :: String -> Action
cmdnotfound str s = do
  putStr str
  putStrLn ": command not found"
  return s





--INTERACTIV

main :: IO ()
main = do
  printwellcome
  toolcycle startState
  
printwellcome :: IO ()
printwellcome = do
  putStrLn "Welcome to RpgTools!"
  

toolcycle :: State -> IO ()
toolcycle state = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin LineBuffering
  putStr "~"
  input <- getLine
  newstate <- (action input) state
  if isQuit newstate
     then return ()
     else toolcycle newstate
  
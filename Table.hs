module Table where

import Dice

import Data.Char
import Data.List.Split

--             wurf name   eintrag  
data Table = T Roll String TableEntry CSV

tableRoll :: Table -> Roll
tableRoll (T r _ _ _) = r

tableName :: Table -> String
tableName (T _ n _ _) = n

tableEntry :: Table -> Int -> String
tableEntry (T _ _ e _) = e

tableCSV :: Table -> CSV
tableCSV (T _ _ _ c) = c

instance Show Table where
  show (T _ _ _ csv) = show csv

type TableEntry = (Int -> String)

emptyTEntry :: TableEntry
emptyTEntry _ = "no valid result"

addTEntry :: (Int, String) -> TableEntry -> TableEntry
addTEntry (x,s) te y  | x == y    = s
                      | otherwise = te y

readTable :: String -> Maybe Table
readTable tabletext = 
  case (reads tabletext) :: [(CSV, String)] of
       ((csv@(CSV ((r:name:_):ess)), _):_)  -> 
              case (reads r) :: [(Roll, String)] of
                   [(roll,_)]     -> Just (T roll name (makeTEs ess) csv)
                   _              -> Nothing
       _                                  -> Nothing
  where
    makeTEs :: [[String]] -> TableEntry
    makeTEs tts = foldr addTEntry emptyTEntry (entryList tts)
    
entryList :: [[String]] -> [(Int, String)]
entryList []     = []
entryList [[]]   = []
entryList (e:es) = let
                    (lo, rest) = case reads (head e) :: [(Int, String)] of
                                      [x]         -> x
                                      otherwise   -> (0,"")
                    hi         = case reads (drop 1 rest) :: [(Int, String)] of
                                      []          -> lo
                                      (y,_):_     -> y
                    in
  [(n,str) | let str = head.(drop 1)$e, n <- [lo..hi]] ++ (entryList es)

dotableroll :: Table -> IO (String)
dotableroll t = do 
    res <- doroll.tableRoll$t
    return ((tableEntry t).resVal$res)

--Datentyp fuer Comma Separated Values:
data CSV = CSV [[String]]
--  deriving Show

--Instanz fuer Show:
instance Show CSV where
  show (CSV xss) = unlines (map separate xss)
    where
      -- Zeilenformatierung:
      separate []     = ""
      separate [x]    = x
      separate (x:xs) = x ++ ['\t'] ++ separate xs

--Instanz fuer Read:
instance Read CSV where
  readsPrec p s = case s of
    ""      -> [ (CSV [[]]          , "") ]
    '\n':s1 -> [ (CSV ([]:xs)     , s2)
                    | (CSV xs      , s2) <- readsPrec p s1 ]
    ',' :s1 -> [ (CSV ((x:xs):xss), s3)
                    | let (x           , s2) = span (\c -> c/=',' && c/='\n') s1
                    , (CSV (xs:xss), s3) <- ((readsPrec p s2)) ]  -- Parser fuer CSV
    _       -> [ (CSV ((x:xs):xss), s2)
                    | let (x           , s1) = span (/=',') s
                    , (CSV (xs:xss), s2) <- ((readsPrec p s1)) ]
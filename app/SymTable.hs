module SymTable where

type Table a = [(String, a)]

-- find the value of a given key in the table
find :: String -> Table a -> Maybe a
find = lookup

-- update the table with a new key-value pais
updt :: String -> a -> Table a -> Table a
updt k v tbl = (k, v) : tbl

-- extend the table with a list of key-value pairs
extd :: [(String, a)] -> Table a -> Table a
extd = foldr (\(k,v) tbl -> updt k v tbl)
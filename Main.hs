--
-- Copyright (c) 2017 Jarra <suhrawardi@gmail.com>
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

module Main where

actionList = [putStr2 "Music\n",
              putStr2 "In dah house!\n"]

putCharList :: String -> [IO()]
putCharList = map putChar

putStr2 :: String -> IO()
putStr2 s = sequence_ (putCharList s)

main :: IO ()
main = sequence_ actionList

--
-- Copyright (c) 2017 Jarra <suhrawardi@gmail.com>
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

module Main where

import Euterpea

t251 :: Music Pitch
t251 = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
           gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
           cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
       in dMinor :+: gMajor :+: cMajor

main :: IO ()
main = play t251

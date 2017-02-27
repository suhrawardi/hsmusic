--
-- Copyright (c) 2017 Jarra <suhrawardi@gmail.com>
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

module Main where

import Euterpea

concertA, a440 ::(PitchClass,Octave)
concertA       = (A, 4) -- concert A
a440           = (A, 4) -- A440

t251 :: Music Pitch
t251 = let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
           gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
           cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
       in dMinor :+: gMajor :+: cMajor

wts :: Pitch -> [ Music Pitch ]
wts p = let f ap = note qn (pitch (absPitch p + ap))
        in map f [0,2,4,6,8]

main :: IO ()
main = play (line (wts a440))

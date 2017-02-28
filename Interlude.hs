--
-- Copyright (c) 2017 Jarra <suhrawardi@gmail.com>
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

module Interlude where

import Euterpea

prefixes :: [a] -> [[a]]
prefixes []      = []
prefixes (x: xs) = let f pf = x:pf
                   in [x]:map f (prefixes xs)

prefix :: [Music a] -> Music a
prefix mel = let m1 = line (concat (prefixes mel))
                 m2 = transpose 12 (line (concat (prefixes (reverse mel))))
                 m = instrument Flute m1 :=: instrument VoiceOohs m2
             in m :+: transpose 5 m :+: m

timesM :: Int -> Music a -> Music a
timesM 0 m = rest 0
timesM n m = m :=: timesM (n - 1) m

addDur :: Dur -> [Dur -> Music a] -> Music a
addDur d ns = let f n = n d
              in (line . map f) ns

graceNote :: Int -> Music Pitch -> Music Pitch
graceNote n (Prim (Note d p)) = note (d/8) (trans n p) :+: note (7 * d/8) p
graceNote n _                 = error "Can only add a single grace note"

b1 = addDur dqn [b 3, fs 4, g 4, fs 4]
b2 = addDur dqn [b 3, es 4, fs 4, es 4]
b3 = addDur dqn [as 3, fs 4, g 4, fs 4]

bassLine = timesM 3 b1 :+: timesM 2 b2 :+: timesM 4 b3 :+: timesM 5 b1

v1 = v1a :+: graceNote 1 (d 5 qn) :+: v1b
v1a = addDur en [a 5, e 5, d 5, fs 5, cs 5, b 4, e 5, b 4]
v1b = addDur en [cs 5, b 4]

mainVoice = timesM 3 v1 :+: v2

v2 = line [cs 5 (dhn + dhn), d 5 dhn, f 5 hn, gs 5 qn, fs 5 (hn + en), g 5 en]

childSong6 :: Music Pitch
childSong6 = let t = (dhn / qn) * (69 / 120)
             in instrument RhodesPiano
                           (tempo t (bassLine :=: mainVoice))

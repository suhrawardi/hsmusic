--
-- Copyright (c) 2017 Jarra <suhrawardi@gmail.com>
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--

module MoreMusic where

import Euterpea.Music

delayM :: Dur -> Music a -> Music a
delayM d m = rest d :+: m

timesM :: Int -> Music a -> Music a
timesM 0 m = rest 0
timesM n m = m :+: timesM (n - 1) m

repeatM :: Music a -> Music a
repeatM m = m :+: repeatM m

lineToList2 :: Music a -> [Music a]
lineToList2 (Prim (Rest 0)) = []
lineToList2 (n :+: ns)      = n:lineToList2 ns
lineToList2 _               =
    error "lineToList2: argument not created by function line"

invert2 :: Music Pitch -> Music Pitch
invert2 m =
  let l@(Prim (Note _ r):_) = lineToList2 m
      inv (Prim (Note d p )) =
                note d (pitch (2 * absPitch r - absPitch p))
      inv (Prim (Rest d))    = rest d
  in line (map inv l)

retro2, retroInvert, invertRetro :: Music Pitch -> Music Pitch
retro2       = line . reverse . lineToList2
retroInvert  = retro2 . invert2
invertRetro  = invert2 . retro2

pr1, pr2 :: Pitch -> Music Pitch
pr1 p = tempo (5/6)
        (tempo (4/3) (mkLn 1 p qn :+:
                      tempo (3/2) (mkLn 3 p en :+:
                                   mkLn 2 p sn :+:
                                   mkLn 1 p qn) :+:
                                   mkLn 1 p qn) :+:
                      tempo (3/2) (mkLn 6 p en))
pr2 p =
  let m1 = tempo (5/4) (tempo (3/2) m2 :+: m2)
      m2 = mkLn 3 p en
  in tempo (7/6) (m1 :+:
                  tempo (5/4) (mkLn 5 p en) :+:
                  m1 :+:
                  tempo (3/2) m2)

mkLn :: Int -> p -> Dur -> Music p
mkLn n p d = line $ replicate n (note d p)

(=:=) :: Dur -> Dur -> Music a -> Music a
old =:= new = tempo (new / old)

dur2 :: Music a -> Dur
dur2 (Prim (Note d _))    = d
dur2 (Prim (Rest d))      = d
dur2 (m1 :+: m2)          = dur2 m1 + dur2 m2
dur2 (m1 :=: m2)          = dur2 m1 `max` dur2 m2
dur2 (Modify (Tempo r) m) = dur2 m / r
dur2 (Modify _ m)         = dur2 m

revM :: Music a -> Music a
revM n@(Prim _)   = n
revM (Modify c m) = Modify c (revM m)
revM (m1 :=: m2)  =
  let d1 = dur2 m1
      d2 = dur2 m2
  in if d1 > d2 then revM m1 :=: (rest (d1 - d2) :+: revM m2)
                else (rest (d2 - d1) :+: revM m1) :=: revM m2

pr12 :: Music Pitch
pr12 = pr1 (C, 4) :=: pr2 (G, 4)

takeM :: Dur -> Music a -> Music a
takeM d m | d <= 0           = rest 0
takeM d (Prim (Note oldD p)) = note (min oldD d) p
takeM d (Prim (Rest oldD))   = rest (min oldD d)
takeM d (m1 :=: m2)          = takeM d m1 :=: takeM d m2
takeM d (m1 :+: m2)          = let m'1 = takeM d m1
                                   m'2 = takeM (d - dur m'1) m2
                               in m'1 :+: m'2
takeM d (Modify (Tempo r) m) = tempo r (takeM (d * r ) m)
takeM d (Modify c m)         = Modify c (takeM d m)

dropM :: Dur -> Music a -> Music a
dropM d m | d <= 0           = rest 0
dropM d (Prim (Note oldD p)) = note (max oldD 0) p
dropM d (Prim (Rest oldD))   = rest (max oldD 0)
dropM d (m1 :=: m2)          = dropM d m1 :=: dropM d m2
dropM d (m1 :+: m2)          = let m'1 = dropM d m1
                                   m'2 = dropM (d - dur m'1) m2
                               in m'1 :+: m'2
dropM d (Modify (Tempo r) m) = tempo r (dropM (d * r ) m)
dropM d (Modify c m)         = Modify c (dropM d m)

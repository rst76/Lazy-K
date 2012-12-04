module Main where

import LazyK.Prim

-- v
v = K :$ (K :$ num 118)

-- ultimate problem. "42"
up = K :$ (delete (V "a") (V "a" :$ (S :$ numB 2 :$ S :$ b) :$ K :$ chr '2'))

-- With the least definitions.
hello0 = K :$ (foldr (consXY . num' . fromEnum) (K :$ num' 256) "Hello, world!")
  where
  num' 1 = I
  num' n = S :$ (S :$ (K :$ S) :$ K) :$ num' (n - 1)

-- With shorter Church Numerals (by 51b).
hello1 = K :$ (foldr (consXY . chr) endOfOutput "Hello, world!")

-- Introduced recursion.
hello2 = K :$ (foldl (\e c -> e :$ (numB (fromEnum c))) (m :$ hh :$ endOfOutput) "!dlrow ,olleH\0")
  where
  hh = delete (V "f") $ delete (V "x") $ delete (V "y") $
    ifnozeroXYZ (V "y" :$ b)
      (V "f" :$ V "f" :$ (consXY (V "y" :$ b) (V "x")))
      (V "x")

-- Defined N as <27> + f <73> <81>.
hello = K :$ (foldl (\e c -> e :$ (nn (fromEnum c))) (m :$ hh :$ K) "!dlrow ,olleH\0")
  where
  hh = delete (V "f") $ delete (V "x") $ delete (V "y") $
    ifnozeroXYZ (V "y" :$ numB 1 :$ numB 1 :$ b)
      (V "f" :$ V "f" :$ (consXY
        (S :$ (S :$ numB 27 :$ S) :$ (V "y" :$ numB 73 :$ numB 81) :$ b)
        (V "x")))
      (V "x")
  nn 0 = K :$ K
  nn 32 = K :$ (K :$ numB 5)
  nn 33 = K :$ (K :$ numB 6)
  nn 44 = K :$ (K :$ numB 17)
  nn 72 = K :$ (K :$ numB 45)
  nn 100 = K
  nn 101 = S :$ (K :$  K) :$ ss
  nn 108 = S :$ K
  nn 111 = K :$ (S :$ (S :$ numB 3 :$ S))
  nn 114 = K :$ (S :$ (S :$ numB 6 :$ S))
  nn 119 = K :$ (S :$ (S :$ numB 11 :$ S))

-- For code golf with restriction on the kind of characters.
hello_sk = replace I (S :$ K :$ K) $ hello

-- main = putStr $ showG v
-- main = putStr $ showG up
main = putStr $ showG hello
-- main = putStr $ showU $ hello_sk

module Main where

import LazyK.Prim

-- v
v = K :$ (K :$ num 118)

-- ultimate problem. "42"
up = K :$ (delete (V "a") (V "a" :$ (S :$ numB 2 :$ S :$ b) :$ K :$ chr '2'))

-- hello world with the least definitions.
hello0 = K :$ (foldr (consXY . num' . fromEnum) (K :$ num' 256) "Hello, world!")
  where
  num' 1 = I
  num' n = S :$ (S :$ (K :$ S) :$ K) :$ num' (n - 1)

-- hello world with shorter Church Numerals (by 51b).
hello1 = K :$ (foldr (consXY . chr) endOfOutput "Hello, world!")

-- hello world with recursion.
hello2 = K :$ (foldl (\e c -> e :$ (numB (fromEnum c))) (m :$ hh :$ endOfOutput) "!dlrow ,olleH\0")
  where
  hh = delete (V "f") $ delete (V "x") $ delete (V "y") $
    ifnozeroXYZ (V "y" :$ b)
      (V "f" :$ V "f" :$ (consXY (V "y" :$ b) (V "x")))
      (V "x")

-- hello world that N is defined as <27> + f <73> <81>.
hello = (foldl (\e c -> e :$ (nn (fromEnum c))) (m :$ hh :$ K) "!dlrow ,olleH\0")
  where
  hh = delete (V "f") $ delete (V "x") $ delete (V "y") $
    ifnozeroXYZ (V "y" :$ numB 1 :$ numB 1 :$ b)
      (V "f" :$ V "f" :$ (consXY
        (S :$ (S :$ numB 27 :$ S) :$ (V "y" :$ numB 73 :$ numB 81) :$ b)
        (V "x")))
      (K :$ V "x")
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

-- Quine
-- code representation : list of modified Church numerals
-- code order : ascending
-- termination judge : end of list
quine0S =  showU $ quine0 $ list $ map qCode0 $ init $ showU $ quine0 I

quine0 code = K :$ (m :$ funcExpr :$ (S :$ S :$ (m :$ codeExpr) :$ (m :$ (+++)) :$ code))
  where

  funcExpr = delete (V "f") $ delete (V "x") $
    (V "x" :$ isNil)
      :$ endOfOutput
      :$ (consXY (expr (carX (V "x"))) (V "f" :$ V "f" :$ cdrX (V "x")))
    where 
    expr x = S :$ (S :$ numB 96 :$ S) :$ (nthXY (x :$ false :$ b) nums :$ numB 9) :$ b
    -- [96,115,107,105] = map (\f -> 96 + f 9) [\x->0, \x->x+(x+1), \x->x+2, \x->x]
    nums = list [K :$ numB 0, S :$ (ssss :$ (K :$ S)) :$ ss, S :$ (K :$ ss) :$ ss, I]

  codeExpr = delete (V "f") $ delete (V "++") $ delete (V "x") $
    (V "x" :$ isNil)
      :$ (append "`kk" nil)
      :$ (V "++"
        :$ (num 2 :$ aas :$ append "i`k" (nthXY (carX (V "x") :$ false :$ b) exprs))
        :$ (ak :$ (V "f" :$ V "f" :$ V "++" :$ cdrX (V "x"))))
    where
    append s e = foldr (consXY . qCode0) e s
    exprs = list [
      consXY (qCode0 'k') nil, -- "k"
      consXY (qCode0 'i') nil, -- "i"
      ass :$ nil, -- "`ss"
      S :$ (K :$ aas) :$ (S :$ (K :$ ak) :$ (num 2 :$ ass)) :$ nil] -- "``s`k`ss`ss"
    aas = S :$ (K :$ (num 2 :$ consX (qCode0 '`'))) :$ consX (qCode0 's') -- ("``s" ++)
    ass = S :$ (K :$ consX (qCode0 '`')) :$ (num 2 :$ consX (qCode0 's')) -- ("`ss" ++)
    ak = S :$ (K :$ consX (qCode0 '`')) :$ consX (qCode0 'k') -- ("`k" ++)

  (+++) = delete (V "f") $ delete (V "x") $ delete (V "y") $
    (V "x" :$ isNil)
      :$ (V "y")
      :$ (consX (carX (V "x")) :$ (V "f" :$ V "f" :$ cdrX (V "x") :$ V "y"))

qCode0 '`' = K
qCode0 's' = I
qCode0 'k' = S :$ S
qCode0 'i' = S :$ (K :$ ss) :$ ss

-- Quine 
-- code representation : 1 - 2 letters (s / ks / kk)
-- code order : descending
-- termination judge : length of '`' succession
quineS = showU quine ++ concatMap qCode (reverse $ showU quine)

quine = replace I (S :$ K :$ K) $ m :$ (
  delete (V "f") $ delete (V "k") $ delete (V "n") $
    ifleq (num 6) (V "n")
      (K :$ (V "k" :$ (K :$ (numB 256 :$ b))))
      (b :$ (delete (V "x") $
        V "f" :$ V "f"
          :$ ((delete (V "y") $ (b :$ (V "n" :$ (K :$ (V "x" :$ I :$ consX (chr '`'))) :$ V "y") :$ (b :$ V "k" :$ V "y")))
            :$ (V "x" :$ consX (chr 'k') :$ consX (chr 's')))
          :$ (V "x" :$ (V "n" :$ num 0) :$ (suc :$ V "n"))
        ) :$ sk2bool)
  ) :$ I :$ num 1

-- SSSK(SKK)K=K, SSSS(SKS)K=KI
sk2bool = delete (V "x") $ S :$ S :$ S :$ V "x" :$ (S :$ K :$ V "x") :$ K

qCode '`' = "s"
qCode 'k' = "kk"
qCode 's' = "ks"

-- main = putStr $ showG v
-- main = putStr $ showG up
-- main = putStr $ showG hello
main =  putStr quineS

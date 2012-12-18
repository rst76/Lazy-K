module Main where

import LazyK.Prim

-- v
v = K :$ (K :$ num 118)

-- "42"
ultimateProblem = K :$ (delete (V "t") (V "t" :$ (S :$ numB 2 :$ S :$ b) :$ K :$ chr '2'))

-- delete blank lines
deleteBlankLines = m :$ dbl
  where
  dbl = delete (V "f") $ delete (V "x") $
    consXY (carX (V "x"))
      (V "f" :$ V "f" :$
        (nthNX (num 20)
          (carX (V "x") :$ suc :$ carX (cdrX (V "x")) :$
              -- car (SSI) = car (cdr (SSI)) = I
              consX I :$ consXY cdr (S :$ S :$ I)) :$
            cdrX (V "x")))

-- even lines
evenLines = m :$ el :$ false
  where
  el = delete (V "f") $ delete (V "t") $ delete (V "x") $
    (ifleMNXY (num 256) (carX (V "x")) true (V "t")) :$ consX (carX (V "x")) :$ I :$
      (m :$ V "f" :$
        (neq10 (carX (V "x")) :$ (V "t") :$ (V "t" :$ false :$ true)) :$
        (cdrX (V "x")))
  -- car (cdr S) = true, car S = false
  neq10 a = nthNX (num 10) (a :$ consX true :$ S)

-- sort characters
sortCharacters = m :$ sort
  where
  sort = delete (V "f") $ delete (V "x") $
    ifleMNXY (num 256) (carX (V "x"))
      (V "x")
      (m :$ ins :$ (m :$ V "f" :$ cdrX (V "x")) :$ (carX (V "x")))
  ins = delete (V "f") $ delete (V "x") $ delete (V "n") $
    ifleMNXY (V "n") (carX (V "x"))
      (consXY (V "n") (V "x"))
      (consXY (carX (V "x")) (m :$ V "f" :$ cdrX (V "x") :$ V "n"))

-- hello world with the least definitions
hello0 = K :$ (foldr (consXY . num' . fromEnum) (K :$ num' 256) "Hello, world!")
  where
  num' 1 = I
  num' n = S :$ (S :$ (K :$ S) :$ K) :$ num' (n - 1)

-- hello world with shorter Church Numerals (by 51b)
hello1 = K :$ (foldr (consXY . chr) endOfOutput "Hello, world!")

-- hello world with recursion
hello2 = K :$ (foldl (\e c -> e :$ (numB (fromEnum c))) (m :$ hh :$ endOfOutput) "!dlrow ,olleH\0")
  where
  hh = delete (V "f") $ delete (V "x") $ delete (V "y") $
    ifnonzeroNXY (V "y" :$ b)
      (V "f" :$ V "f" :$ (consXY (V "y" :$ b) (V "x")))
      (V "x")

-- hello world that N is defined as <27> + f <73> <81>
hello = (foldl (\e c -> e :$ (nn (fromEnum c))) (m :$ hh :$ K) "!dlrow ,olleH\0")
  where
  hh = delete (V "f") $ delete (V "x") $ delete (V "y") $
    ifnonzeroNXY (V "y" :$ numB 1 :$ numB 1 :$ b)
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

fizzBuzz = K :$ (m :$ fb :$ num 1)
fb = delete (V "f") $ delete (V "x") $
  ifleMNXY (V "x") (num 100)
    ((delete (V "y") $ V "y" :$ nil :$ isNil :$ (show10 :$ V "x") :$ V "y") :$
      fizzbuzz (V "x") :$ consXY (num 10) (m :$ V "f" :$ (suc :$ V "x")))
    K
  where
  fizzbuzz n = S :$ (S :$ (K :$ b) :$ nthNX n (m :$ fizz)) :$ nthNX n (m :$ buzz) :$
    (num 2 :$ consX (num 122))
  fizz = delete (V "f") $
    consXY
      (S :$ (K :$ (S :$ (K :$ consX (num 70)) :$ consX (num 105))))
      (num 2 :$ consX false :$ (m :$ V "f"))
  buzz = delete (V "f") $
    consXY
      (S :$ (K :$ (S :$ (K :$ consX (num 66)) :$ consX (num 117))))
      (num 4 :$ consX false :$ (m :$ V "f"))
  show10 = m :$ dec :$ num 0
  dec = delete (V "f") $ delete (V "x") $ delete (V "y") $
    ifleMNXY (num 10) (V "y")
      (m :$ V "f" :$ (suc :$ V "x") :$ (num 10 :$ pre :$ V "y"))
      (b :$
        (ifnonzeroNXY (V "x") (m :$ V "f" :$ num 0 :$ V "x") I) :$
        (consX (S :$ numB 48 :$ S :$ b :$ V "y")))

-- Quine
-- code representation : list of modified Church numerals
-- code order : ascending
-- termination judge : end of list
quine0S =  showU $ quine0 $ list $ map qCode0 $ init $ showU $ quine0 I

quine0 code = K :$ (m :$ funcExpr :$ (S :$ S :$ (m :$ codeExpr) :$ (m :$ (+++)) :$ code))
  where

  funcExpr = delete (V "f") $ delete (V "x") $
    (V "x" :$ isNil) :$
      endOfOutput :$
      (consXY (expr (carX (V "x"))) (V "f" :$ V "f" :$ cdrX (V "x")))
    where 
    expr x = S :$ (S :$ numB 96 :$ S) :$ (nthNX (x :$ false :$ b) nums :$ numB 9) :$ b
    -- [96,115,107,105] = map (\f -> 96 + f 9) [\x->0, \x->x+(x+1), \x->x+2, \x->x]
    nums = list [K :$ numB 0, S :$ (ssss :$ (K :$ S)) :$ ss, S :$ (K :$ ss) :$ ss, I]

  codeExpr = delete (V "f") $ delete (V "++") $ delete (V "x") $
    (V "x" :$ isNil) :$
      (append "`kk" nil) :$
      (V "++" :$
        (num 2 :$ aas :$ append "i`k" (nthNX (carX (V "x") :$ false :$ b) exprs)) :$
        (ak :$ (V "f" :$ V "f" :$ V "++" :$ cdrX (V "x"))))
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
    (V "x" :$ isNil) :$
      (V "y") :$
      (consX (carX (V "x")) :$ (V "f" :$ V "f" :$ cdrX (V "x") :$ V "y"))

qCode0 '`' = K
qCode0 's' = I
qCode0 'k' = S :$ S
qCode0 'i' = S :$ (K :$ ss) :$ ss

-- Quine 
-- code representation : 1 - 2 letters (s / ks / kk)
-- code order : descending
-- termination judge : length of '`' succession
quineS = showU quine ++ concatMap qCode (reverse $ showU quine)

quine = replace I (S :$ K :$ K) $ m :$ q :$ I :$ num 1
  where
  q = delete (V "f") $ delete (V "k") $ delete (V "n") $
    ifleMNXY (num 6) (V "n")
      -- for anarchy golf
      -- (V "k")
      (K :$ (V "k" :$ (K :$ (numB 256 :$ b))))
      (b :$
        (delete (V "x") $
          V "f" :$ V "f" :$
            ((delete (V "y") $ (b :$
                (V "n" :$ (K :$ (V "x" :$ I :$ consX (chr '`'))) :$ V "y") :$
                (b :$ V "k" :$ V "y"))) :$
              (V "x" :$ consX (chr 'k') :$ consX (chr 's'))) :$
            (V "x" :$ (V "n" :$ num 0) :$ (suc :$ V "n"))) :$
        sk2bool)

-- (\x->SSSx(SKx)K)K = true, (\x->SSSx(SKx)K)S = false
sk2bool = delete (V "x") $ S :$ S :$ S :$ V "x" :$ (S :$ K :$ V "x") :$ K

qCode '`' = "s"
qCode 'k' = "kk"
qCode 's' = "ks"

-- main = putStr $ showG hello
main =  putStr quineS

module LazyK.Prim where

import Control.Applicative
import Text.Parsec hiding ((<|>))
import Text.Parsec.String

data Expr = S | K | I | Expr :$ Expr | Succ | Num Int | V String deriving (Eq, Read, Show)
data Printer = Combinator | Unlambda | Iota | Golf

expr :: Parser Expr
expr = S <$ oneOf "sS"
  <|>  K <$ oneOf "kK"
  <|>  I <$ oneOf "iI"
  <|>  (:$) <$ char '`' <*> expr <*> expr
  <|>  char '(' *> ccExpr <* char ')'

ccExpr :: Parser Expr
ccExpr = foldl1 (:$) <$> many1 expr

parseExpr :: String -> Expr
parseExpr str = case parse ccExpr "" str of
    Right x -> x
    Left  e -> error (show e)

showExpr :: Printer -> Expr -> String
showExpr Iota (S :$ K) = "*i*ii"
showExpr Iota S = "*i*i*i*ii"
showExpr Unlambda S = "s"
showExpr _ S = "S"
showExpr Iota K = "*i*i*ii"
showExpr Unlambda K = "k"
showExpr _ K = "K"
showExpr Iota I = "*ii"
showExpr Unlambda I = "i"
showExpr _ I = "I"
showExpr _ Succ = "<Succ>"
showExpr _ (V v) = "<" ++ v ++ ">"
showExpr _ (Num i) = "<" ++ show i ++ ">"
showExpr Iota (x :$ y) = '*' : showExpr Iota x ++ showExpr Iota y
showExpr Unlambda (x :$ y) = '`' : showExpr Unlambda x ++ showExpr Unlambda y
showExpr p (x :$ y@(_ :$ _ :$ _)) = showExpr p x ++ "(" ++ showExpr p y ++ ")"
showExpr Golf (x :$ y@(_ :$ _)) = showExpr Golf x ++ '`' : showExpr Golf y
showExpr Combinator (x :$ y@(_ :$ _)) = showExpr Combinator x ++ "(" ++ showExpr Combinator y ++ ")"
showExpr p (x :$ y) = showExpr p x ++ showExpr p y

showC = showExpr Combinator
showI = showExpr Iota
showG = showExpr Golf
showU = showExpr Unlambda

ss = S :$ S
si = S :$ I
sss = ss :$ S
ssi = ss :$ I
ssf = ss :$ false
ssss1 = S :$ sss
ssss = ss :$ S :$ S
ssiss = S :$ (S :$ I :$ S) :$ S
ssssss = ssss :$ S :$ S

b = S :$ (K :$ S) :$ K
m = S :$ I :$ I
t = S :$ (K :$ si) :$ K

true = K
false = S :$ K

consXY x y = S :$ (si :$ (K :$ x)) :$ (K :$ y)
consX x = S :$ (K :$ (S :$ (si :$ (K :$ x)))) :$ K
cons = S :$ (S :$ (K :$ S) :$ (S :$ (K :$ K) :$ (S :$ (K :$ S) :$ (S :$ (K :$ si) :$ K)))) :$ (K :$ K)

nil = K :$ true
isNil = K :$ (K :$ false)
car = true
cdr = false
carX x = x :$ true
cdrX x = x :$ false
nthXY x y = x :$ (si :$ (K :$ cdr)) :$ y :$ car

list :: [Expr] -> Expr
list = foldr consXY nil

suc = S :$ b

ifnozeroXYZ x y z = x :$ (K :$ y) :$ z
ifleq m n x y = (m :$ t :$ (K :$ x)) :$ (n :$ t :$ (K :$ y))

chr :: Char -> Expr
chr = num . fromEnum

endOfOutput = K :$ num 256

num :: Int -> Expr
num 0 = S :$ K
num 1 = I
num 2 = suc :$ I
num 4 = m :$ num 2
num 8 = S :$ (S :$ b) :$ I :$ num 2
num 16 = si :$ m :$ num 2
num 256 = m :$ num 4
num n = numB n :$ b

numB 0 = K :$ false
numB 1 = false
numB 2 = ss :$ numB 1
numB 3 = ss :$ numB 2
numB 4 = ssi :$ numB 2
numB 5 = ss :$ numB 4
numB 6 = ssss1 :$ numB 2
numB 7 = ss :$ numB 6
numB 8 = S :$ (S :$ (K :$ S) :$ ss) :$ I :$ numB 2
numB 9 = ssss :$ numB 2
numB 10 = ss :$ numB 9
numB 11 = ss :$ numB 10
numB 12 = S :$ (ss :$ sss) :$ numB 2
numB 13 = ss :$ numB 12
numB 14 = ss :$ numB 13
numB 15 = S :$ (ss :$ ssiss) :$ numB 3
numB 16 = ss :$ ssi :$ numB 2
numB 17 = ss :$ numB 16
numB 18 = ss :$ numB 17
numB 19 = ss :$ numB 18
numB 20 = ssss1 :$ numB 4
numB 21 = ss :$ numB 20
numB 22 = ss :$ numB 21
numB 23 = ss :$ numB 22
numB 24 = S :$ (ss :$ (ss :$ sss)) :$ numB 2
numB 25 = ss :$ (S :$ (K :$ ss) :$ ssi) :$ numB 2
numB 26 = ss :$ numB 25
numB 27 = ssi :$ numB 3
numB 28 = ss :$ numB 27
numB 29 = ss :$ numB 28
numB 30 = ssss1 :$ numB 5
numB 31 = ss :$ numB 30
numB 32 = S :$ numB 5 :$ numB 2
numB 33 = ss :$ numB 32
numB 34 = ss :$ numB 33
numB 35 = S :$ (ss :$ ssiss) :$ numB 5
numB 36 = ss :$ ssss1 :$ numB 2
numB 37 = ss :$ numB 36
numB 38 = ss :$ numB 37
numB 39 = ss :$ numB 38
numB 40 = ss :$ numB 39
numB 41 = ss :$ numB 40
numB 42 = ssss1 :$ numB 6
numB 43 = ss :$ numB 42
numB 44 = ss :$ numB 43
numB 45 = S :$ (ss :$ (ss :$ ssiss)) :$ numB 3
numB 46 = ss :$ numB 45
numB 47 = ss :$ numB 46
numB 48 = S :$ (ss :$ (ss :$ (ss :$ sss))) :$ numB 2
numB 49 = ss :$ (S :$ (K :$ ss) :$ ssss1) :$ numB 2
numB 50 = ss :$ numB 49
numB 51 = ss :$ numB 50
numB 52 = ss :$ numB 51
numB 53 = ss :$ numB 52
numB 54 = S :$ (si :$ numB 2) :$ numB 27
numB 55 = ss :$ numB 54
numB 56 = ssss1 :$ numB 7
numB 57 = ss :$ numB 56
numB 58 = ss :$ numB 57
numB 59 = ss :$ numB 58
numB 60 = S :$ (si :$ numB 2) :$ numB 30
numB 61 = ss :$ numB 60
numB 62 = S :$ (si :$ numB 2) :$ numB 31
numB 63 = S :$ (ss :$ ssiss) :$ numB 7
numB 64 = ssss :$ numB 3
numB 65 = ss :$ numB 64
numB 66 = ss :$ numB 65
numB 67 = ss :$ numB 66
numB 68 = ss :$ numB 67
numB 69 = ss :$ numB 68
numB 70 = S :$ (S :$ numB 6 :$ S) :$ numB 64
numB 71 = ss :$ numB 70
numB 72 = ssss1 :$ numB 8
numB 73 = ss :$ numB 72
numB 74 = ss :$ numB 73
numB 75 = ss :$ numB 74
numB 76 = S :$ (si :$ numB 2) :$ numB 38
numB 77 = ss :$ numB 76
numB 78 = S :$ (si :$ numB 2) :$ numB 39
numB 79 = ss :$ numB 78
numB 80 = S :$ (ss :$ sss) :$ numB 4
numB 81 = ssssss :$ numB 2
numB 82 = ss :$ numB 81
numB 83 = ss :$ numB 82
numB 84 = ss :$ numB 83
numB 85 = ss :$ numB 84
numB 86 = ss :$ numB 85
numB 87 = S :$ (S :$ numB 6 :$ S) :$ numB 81
numB 88 = ss :$ numB 87
numB 89 = S :$ (S :$ numB 8 :$ S) :$ numB 81
numB 90 = ssss1 :$ numB 9
numB 91 = ss :$ numB 90
numB 92 = ss :$ numB 91
numB 93 = ss :$ numB 92
numB 94 = ss :$ numB 93
numB 95 = ss :$ numB 94
numB 96 = S :$ (ss :$ (ss :$ (ss :$ (ss :$ sss)))) :$ numB 2
numB 97 = ss :$ numB 96
numB 98 = ss :$ numB 97
numB 99 = S :$ (ss :$ ssiss) :$ numB 9
numB 100 = S :$ numB 2 :$ numB 10
numB 101 = ss :$ numB 100
numB 102 = ss :$ numB 101
numB 103 = ss :$ numB 102
numB 104 = ss :$ numB 103
numB 105 = S :$ (S :$ numB 24 :$ S) :$ numB 81
numB 106 = S :$ (S :$ numB 16 :$ S) :$ numB 90
numB 107 = S :$ (S :$ numB 27 :$ S) :$ numB 80
numB 108 = S :$ (ss :$ (ss :$ sss)) :$ numB 3
numB 109 = ss :$ numB 108
numB 110 = ssss1 :$ numB 10
numB 111 = ss :$ numB 110
numB 112 = ss :$ numB 111
numB 113 = ss :$ numB 112
numB 114 = ss :$ numB 113
numB 115 = ss :$ numB 114
numB 116 = S :$ (si :$ numB 4) :$ numB 29
numB 117 = S :$ (S :$ numB 36 :$ S) :$ numB 81
numB 118 = ss :$ numB 117
numB 119 = S :$ (si :$ numB 7) :$ numB 17
numB 120 = S :$ (ss :$ ssiss) :$ numB 10
numB 121 = S :$ numB 2 :$ numB 11
numB 122 = ss :$ numB 121
numB 123 = ss :$ numB 122
numB 124 = ss :$ numB 123
numB 125 = ss :$ (S :$ (K :$ ss) :$ ss) :$ numB 3
numB 126 = ss :$ numB 125
numB 127 = ss :$ numB 126
numB 128 = S :$ numB 7 :$ numB 2
numB 256 = ssi :$ numB 4

len :: Expr -> Int
len (x :$ y) = len x + len y
len _ = 1

elem' :: Expr -> Expr -> Bool
elem' v x | v == x = True
elem' v (x :$ y) = elem' v x ||  elem' v y
elem' v _ = False

gather :: Expr -> Expr -> Expr
gather v x | not (elem' v x) = x
gather v (x :$ y) | not (elem' v y) = gather v x :$ y
gather v (x :$ y) | not (elem' v x) = x :$ gather v y
gather v (x :$ y) = S :$ delete v x :$ delete v y :$ v

delete :: Expr -> Expr -> Expr
delete v (x :$ y) | elem' v x && elem' v y = S :$ delete v x :$ delete v y
delete v (x :$ y) | elem' v x = S :$ delete v x :$ (K :$ y)
delete v (x :$ y) | v == y = x
delete v (x :$ y) | elem' v y = S :$ (K :$ x) :$ delete v y
delete v x | v == x = I

replace :: Expr -> Expr -> Expr -> Expr
replace a b x | a == x = b
replace a b (x :$ y) = replace a b x :$ replace a b y
replace a b x = x

apply :: Expr -> Expr -> Expr
apply (S :$ x :$ y) z = apply (apply x z) (apply y z)
apply (K :$ x) y = x
apply I x = x
apply Succ (Num i) = Num (i + 1)
apply x y = x :$ y

reduce :: Expr -> Expr
reduce (x :$ y) = apply (reduce x) (reduce y)
reduce x = x

run :: Expr -> IO ()
run expr = case apply (apply (apply expr car) Succ) (Num 0) of
  Num 256 -> return ()
  Num i -> putChar (toEnum i) >> run (apply expr cdr)
  x -> error ("Output should be the list of Church numerals: " ++ showC x)

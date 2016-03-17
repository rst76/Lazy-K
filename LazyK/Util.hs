module LazyK.Util where

import Prelude (($))
import LazyK.Prim

lambda s = delete (V s)

and = lambda "p" $ lambda "q" $ V "p" :$ V "q" :$ V "p"
or  = lambda "p" $ lambda "q" $ V "p" :$ V "p" :$ V "q"
not = lambda "p" $ lambda "a" $ lambda "b" $ V "p" :$ V "b" :$ V "a"

le = lambda "m" $ lambda "n" $ lambda "x" $ lambda "y" $ ifleMNXY (V "m") (V "n") (V "x") (V "y")

eq = lambda "m" $ lambda "n" $ and :$ (le :$ V "m" :$ V "n") :$ (le :$ V "n" :$ V "m")

plus = lambda "m" $ lambda "n" $ lambda "f" $ lambda "x" $
  V "m" :$ V "f" :$ (V "n" :$ V "f" :$ V "x")

mult = lambda "m" $ lambda "n" $ lambda "f" $
  V "m" :$ (V "n" :$ V "f")

pow = lambda "m" $ lambda "n" $ V "n" :$ V "m"

succ = lambda "n" $ lambda "f" $ lambda "x" $
  V "n" :$ V "f" :$ (V "f" :$ V "x")

pred = lambda "n" $ lambda "f" $ lambda "x" $
  V "n" :$ (lambda "g" $ lambda "h" $ V "h" :$ (V "g" :$ V "f")) :$ (K :$ V "x") :$ I

sub = lambda "m" $ lambda "n" $ V "n" :$ pred :$ V "m"

divMod = lambda "m" $ lambda "n" $
  m :$ (lambda "r" $ lambda "q" $ lambda "m" $
      ifleMNXY (V "n") (V "m") (m :$ V "r" :$ (succ :$ V "q") :$ (sub :$ V "m" :$ V "n")) (cons :$ V "q" :$ V "m"))
    :$ num 0 :$ V "m"

div = lambda "m" $ lambda "n" $ car :$ (divMod :$ V "m" :$ V "n")

mod = lambda "m" $ lambda "n" $ cdr :$ (divMod :$ V "m" :$ V "n")

repeat = m :$ (lambda "f" $ lambda "x" $ consXY (V "x") (V "f" :$ V "f" :$ V "x"))

take = (m :$) $ lambda "r" $ lambda "n" $ lambda "x" $
  ifnonzeroNXY (V "n") (V "x" :$ isNil :$ nil :$ (cons :$ (car :$ V "x") :$ (m :$ V "r" :$ (pred :$ V "n") :$ (cdr :$ V "x")))) nil

drop = (m :$) $ lambda "r" $ lambda "n" $ lambda "x" $
  ifnonzeroNXY (V "n") (V "x" :$ isNil :$ nil :$ (m :$ V "r" :$ (pred :$ V "n") :$ (cdr :$ V "x"))) (V "x")

reverse = m :$ (lambda "r" $ lambda "a" $ lambda "x" $
  V "x" :$ isNil :$ V "a" :$ (m :$ V "r" :$ (cons :$ (car :$ V "x") :$ V "a") :$ (cdr :$ V "x"))) :$ nil

takeWhile = (m :$) $ lambda "r" $ lambda "p" $ lambda "x" $
  V "x" :$ isNil :$ nil :$ (V "p" :$ (car :$ V "x") :$ (cons :$ (car :$ V "x") :$ (m :$ V "r" :$ V "p" :$ (cdr :$ V "x"))) :$ nil)

foldr = (m :$) $ lambda "r" $ lambda "f" $ lambda "z" $ lambda "x" $
  V "x" :$ isNil :$ V "z" :$ (V "f" :$ (car :$ V "x") :$ (m :$ V "r" :$ V "f" :$ V "z" :$ (cdr :$ V "x")))

foldrC2 = (m :$) $ lambda "r" $ lambda "f" $ lambda "x" $ lambda "z" $
  V "x" :$ isNil :$ V "z" :$ (V "f" :$ (car :$ V "x") :$ (m :$ V "r" :$ V "f" :$ (cdr :$ V "x") :$ V "z"))

append = foldrC2 :$ cons

concatMap = lambda "f" $ foldr :$ (b :$ append :$ V "f") :$ nil

zipWith = (m :$) $ lambda "r" $ lambda "f" $ lambda "x" $ lambda "y" $
  V "x" :$ isNil :$ nil :$ (V "y" :$ isNil :$ nil
  :$ (cons :$ (V "f" :$ (car :$ V "x") :$ (car :$ V "y")) :$ (m :$ V "r" :$ V "f" :$ (cdr :$ V "x") :$ (cdr :$ V "y"))))

length = foldr :$ (K :$ succ) :$ num 0

ands = foldr :$ and :$ true

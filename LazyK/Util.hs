module LazyK.Util where

import Prelude (($))
import LazyK.Prim

lambda s = delete (V s)

and = delete (V "p") $ delete (V "q") $ V "p" :$ V "q" :$ V "p"
or  = delete (V "p") $ delete (V "q") $ V "p" :$ V "p" :$ V "q"
not = delete (V "p") $ delete (V "a") $ delete (V "b") $ V "p" :$ V "b" :$ V "a"

le = delete (V "m") $ delete (V "n") $ delete (V "x") $ delete (V "y") $ ifleMNXY (V "m") (V "n") (V "x") (V "y")

eq = delete (V "m") $ delete (V "n") $ and :$ (le :$ V "m" :$ V "n") :$ (le :$ V "n" :$ V "m")

plus = delete (V "m") $ delete (V "n") $ delete (V "f") $ delete (V "x") $
  V "m" :$ V "f" :$ (V "n" :$ V "f" :$ V "x")

mult = delete (V "m") $ delete (V "n") $ delete (V "f") $
  V "m" :$ (V "n" :$ V "f")

pow = delete (V "m") $ delete (V "n") $ V "n" :$ V "m"

succ = delete (V "n") $ delete (V "f") $ delete (V "x") $
  V "n" :$ V "f" :$ (V "f" :$ V "x")

pred = delete (V "n") $ delete (V "f") $ delete (V "x") $
  V "n" :$ (delete (V "g") $ delete (V "h") $ V "h" :$ (V "g" :$ V "f")) :$ (K :$ V "x") :$ I

sub = delete (V "m") $ delete (V "n") $ V "n" :$ pred :$ V "m"

divMod = delete (V "m") $ delete (V "n") $
  m :$ (delete (V "r") $ delete (V "q") $ delete (V "m") $
      ifleMNXY (V "n") (V "m") (m :$ V "r" :$ (succ :$ V "q") :$ (sub :$ V "m" :$ V "n")) (cons :$ V "q" :$ V "m"))
    :$ num 0 :$ V "m"

div = delete (V "m") $ delete (V "n") $ car :$ (divMod :$ V "m" :$ V "n")

mod = delete (V "m") $ delete (V "n") $ cdr :$ (divMod :$ V "m" :$ V "n")

append = flipFoldr :$ cons

flipFoldr = (m :$) $ delete (V "r") $ delete (V "f") $ delete (V "x") $ delete (V "z") $
  V "x" :$ isNil :$ V "z" :$ (V "f" :$ (car :$ V "x") :$ (m :$ V "r" :$ V "f" :$ (cdr :$ V "x") :$ V "z"))

foldr = (m :$) $ delete (V "r") $ delete (V "f") $ delete (V "z") $ delete (V "x") $
  V "x" :$ isNil :$ V "z" :$ (V "f" :$ (car :$ V "x") :$ (m :$ V "r" :$ V "f" :$ V "z" :$ (cdr :$ V "x")))

repeat = listOf

take = (m :$) $ delete (V "r") $ delete (V "n") $ delete (V "x") $
  ifnonzeroNXY (V "n") (V "x" :$ isNil :$ nil :$ (cons :$ (car :$ V "x") :$ (m :$ V "r" :$ (pred :$ V "n") :$ (cdr :$ V "x")))) nil

drop = (m :$) $ delete (V "r") $ delete (V "n") $ delete (V "x") $
  ifnonzeroNXY (V "n") (V "x" :$ isNil :$ nil :$ (m :$ V "r" :$ (pred :$ V "n") :$ (cdr :$ V "x"))) (V "x")

reverse = m :$ (delete (V "r") $ delete (V "a") $ delete (V "x") $
  V "x" :$ isNil :$ V "a" :$ (m :$ V "r" :$ (cons :$ (car :$ V "x") :$ V "a") :$ (cdr :$ V "x"))) :$ nil

takeWhile = (m :$) $ delete (V "r") $ delete (V "p") $ delete (V "x") $
  V "x" :$ isNil :$ nil :$ (V "p" :$ (car :$ V "x") :$ (cons :$ (car :$ V "x") :$ (m :$ V "r" :$ V "p" :$ (cdr :$ V "x"))) :$ nil)

flip = delete (V "f") $ delete (V "x") $ delete (V "y") $ V "f" :$ V "y" :$ V "x"

concatMap = delete (V "f") $ foldr :$ (b :$ append :$ V "f") :$ nil

zipWith = (m :$) $ delete (V "r") $ delete (V "f") $ delete (V "x") $ delete (V "y") $
  V "x" :$ isNil :$ nil :$ (V "y" :$ isNil :$ nil
  :$ (cons :$ (V "f" :$ (car :$ V "x") :$ (car :$ V "y")) :$ (m :$ V "r" :$ V "f" :$ (cdr :$ V "x") :$ (cdr :$ V "y"))))

length = foldr :$ (K :$ succ) :$ num 0

all = foldr :$ and :$ true

eof = endOfOutput

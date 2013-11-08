import Data.Function
import Data.List
import LazyK.Prim

-- (1 +) (X B) = S B (X B) = S S X B
-- (X B) + (Y B) = X B (S B) (Y B) = S (S X S) Y B
-- (X B) * (Y B) = B (X B) (Y B) = S (S I X) Y B
-- (X B) ^ (Y B) = (Y B) (X B) = S Y X B
-- (X B) * F B (X B) = S (S S F) X B
-- (F X B) ^ (X B) = S X (F X) B = S S F X B

nb 0 = (K :$ (S :$ K), "K :$ false")
nb 1 = (S :$ K, "false")
nb n = minimumBy (compare `on` (\(e, s) -> (length $ showG e, len e))) $
  [(ssi :$ (bs!!2), "ssi :$ numB 2") | n == 4] ++
  [(ssi :$ (bs!!3), "ssi :$ numB 3") | n == 27] ++
  [(ssi :$ (bs!!4), "ssi :$ numB 4") | n == 256] ++
  [(S :$ (S :$ (K :$ S) :$ ss) :$ I :$ (bs!!2), "S :$ (S :$ (K :$ S) :$ ss) :$ I :$ numB 2") | n == 8] ++
  [(ssss :$ (bs!!2), "ssss :$ numB 2") | n == 9] ++
  [(ssss :$ (bs!!3), "ssss :$ numB 3") | n == 64] ++
  [(ss :$ ssi :$ (bs!!2), "ss :$ ssi :$ numB 2") | n == 16] ++
  [(S :$ (ss :$ sss) :$ (bs!!2), "S :$ (ss :$ sss) :$ numB 2") | n == 12] ++
  [(S :$ (ss :$ sss) :$ (bs!!3), "S :$ (ss :$ sss) :$ numB 3") | n == 36] ++
  [(S :$ (ss :$ sss) :$ (bs!!4), "S :$ (ss :$ sss) :$ numB 4") | n == 80] ++
  [(S :$ (ss :$ (ss :$ sss)) :$ (bs!!2), "S :$ (ss :$ (ss :$ sss)) :$ numB 2") | n == 24] ++
  [(S :$ (ss :$ (ss :$ sss)) :$ (bs!!3), "S :$ (ss :$ (ss :$ sss)) :$ numB 3") | n == 108] ++
  [(ss :$ ssss1 :$ (bs!!2), "ss :$ ssss1 :$ numB 2") | n == 36] ++
  [(ssssss :$ (bs!!2), "ssssss :$ numB 2") | n == 81] ++
  [(S :$ (ss :$ (ss :$ (ss :$ sss))) :$ (bs!!2), "S :$ (ss :$ (ss :$ (ss :$ sss))) :$ numB 2") | n == 48] ++
  [(S :$ (ss :$ (ss :$ (ss :$ (ss :$ sss)))) :$ (bs!!2), "S :$ (ss :$ (ss :$ (ss :$ (ss :$ sss)))) :$ numB 2") | n == 96] ++
  [(ss :$ (S :$ (K :$ ss) :$ ss) :$ (bs!!3), "ss :$ (S :$ (K :$ ss) :$ ss) :$ numB 3") | n == 125] ++
  [(ss :$ (S :$ (K :$ ss) :$ ssi) :$ (bs!!2), "ss :$ (S :$ (K :$ ss) :$ ssi) :$ numB 2") | n == 25] ++
  [(ss :$ (S :$ (K :$ ss) :$ ssss1) :$ (bs!!2), "ss :$ (S :$ (K :$ ss) :$ ssss1) :$ numB 2") | n == 49] ++
  [(S :$ (ss :$ (ss :$ ssiss)) :$ (bs!!3), "S :$ (ss :$ (ss :$ ssiss)) :$ numB 3") | n == 45] ++
  [(S :$ (ss :$ (ss :$ ssiss)) :$ (bs!!3), "S :$ (ss :$ (ss :$ ssiss)) :$ numB 4") | n == 84] ++
  [(S :$ S :$ (bs!!(n-1)), "ss :$ numB " ++ show (n-1))] ++
  [(S :$ (S :$ (bs!!i) :$ S) :$ (bs!!j), "S :$ (S :$ numB " ++ show i ++ " :$ S) :$ numB " ++ show j) |
    i <- [2..n-2], let j = n - i] ++
  [(S :$ (S :$ I :$ (bs!!i)) :$ (bs!!j), "S :$ (si :$ numB " ++ show i ++ ") :$ numB " ++ show j) |
    i <- [2..div n 2], (j, 0) <- [divMod n i]] ++
  [(S :$ (bs!!j) :$ (bs!!i), "S :$ numB " ++ show j ++ " :$ numB " ++ show i) |
    i <- [2..floor(sqrt(toEnum n))], let j = round $ log (toEnum n) / log (toEnum i), i^j == n] ++
  [(S :$ (ss :$ ssiss) :$ (bs!!i), "S :$ (ss :$ ssiss) :$ numB " ++ show i) |
    let i = fromEnum$(sqrt(toEnum(n+1))-1), i*(i+2) == n] ++
  [(ssss1 :$ (bs!!i), "ssss1 :$ numB " ++ show i) |
    let i = fromEnum$(sqrt(toEnum(4*n+1))-1)/2, i*(i+1) == n]

bs = map (fst . nb) [0..]

main = mapM_ putStrLn ["numB " ++ show n ++ " = " ++ s | n <- [0..128], let (_, s) = nb n]

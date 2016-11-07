{-# LANGUAGE ViewPatterns #-}
import Data.Ord
import Data.Sequence as S

--incrocia :: Num a => S.Seq a -> S.Seq a -> S.Seq a
incrocia (viewl -> x :< (viewl -> EmptyL)) _ = empty
incrocia _ (viewl -> y :< (viewl -> EmptyL)) = empty
incrocia (viewl -> x0 :< (viewl -> x1 :< xs)) (viewl -> y0 :< (viewl -> y1 :< ys)) 
    | (x0 < y0) && (x1 > y1) = -x1 <| incrocia (x1 <| xs) (y1 <| ys)
    | (x0 > y0) && (x1 < y1) = x1  <| incrocia (x1 <| xs) (y1 <| ys)
    | otherwise              = incrocia (x1 <| xs) (y1 <| ys)
--    where x0 = grezzo `index` 0
--          x1 = grezzo `index` 1
--          y0 = media `index` 0
--          y1 = media `index` 1
main = print $ incrocia (S.fromList [10,1,10]) (S.fromList [1,3,2])

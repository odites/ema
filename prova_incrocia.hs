{-# LANGUAGE ViewPatterns #-}
import Data.Ord
import Data.Sequence as S

incrocia :: Num a => S.Seq a -> S.Seq a -> S.Seq a
incrocia (viewl -> x :< empty) _ = empty
incrocia _ (viewl -> y :< empty) = empty
incrocia (viewl -> x0 :< (viewl -> x1 :< xs)) (viewl -> y0 :< (viewl -> y1 :< ys)) = x1 <| (incrocia (x1 <| xs) (y1 <| ys))
--    | (x0 < y0) = 5.0 <| incrocia (S.drop 1 grezzo) (S.drop 1 media)
--    | (x0 > y0) = 10.0 <| incrocia (S.drop 1 grezzo) (S.drop 1 media)
--    | otherwise              = incrocia (S.drop 1 grezzo) (S.drop 1 media)
--    where x0 = grezzo `index` 0
--          x1 = grezzo `index` 1
--          y0 = media `index` 0
--          y1 = media `index` 1
main = print $ incrocia (S.fromList [2,1,3]) (S.fromList [1,3,2])
-- (viewl -> x :< (viewl -> y :< (viewl -> EmptyL)))

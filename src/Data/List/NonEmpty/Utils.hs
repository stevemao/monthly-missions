module Data.List.NonEmpty.Utils where
import           Data.List
import           Data.List.NonEmpty (NonEmpty ((:|)), toList)

upsertList :: Eq k => (Maybe v -> v) -> k -> [(k, v)] -> NonEmpty (k, v)
upsertList f k m = case findValueIndex (\(key, _) -> key == k) m of
  Nothing              -> (k, f Nothing) :| m
  Just ((_, v), i, ne) -> replace i (k, f (Just v)) ne

findValueIndex :: (a -> Bool) -> [a] -> Maybe (a, Int, NonEmpty a)
findValueIndex _ [] = Nothing
findValueIndex f x@(a : as) = case find (f . fst) (zip x [0..]) of
  Nothing     -> Nothing
  Just (b, i) -> Just (b, i, a :| as)

replace :: Int -> a -> NonEmpty a -> NonEmpty a
replace 0 a (_:|xs) = a :| xs
replace n a (x:|(x':xs)) =
  if n < 0
    then x :| (x':xs)
    else x :| toList (replace (n-1) a (x' :| xs))
replace _ _ (x:|[]) = x :| []

foldr2NonEmpty :: (a -> [b] -> NonEmpty b) -> NonEmpty a -> NonEmpty b
foldr2NonEmpty f (a :| [])       = f a []
foldr2NonEmpty f (a :| (b : bs)) = f a $ toList $ foldr2NonEmpty f (b :| bs)

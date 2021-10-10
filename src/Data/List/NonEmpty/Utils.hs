module Data.List.NonEmpty.Utils where
import           Data.List
import           Data.List.NonEmpty (NonEmpty ((:|)))

upsertList :: Eq k => (Maybe v -> v) -> k -> [(k, v)] -> NonEmpty (k, v)
upsertList f k m = case findValueIndex (\(key, _) -> key == k) m of
  Nothing              -> (k, f Nothing) :| m
  Just ((_, v), i, ne) -> replace' ne i (k, f (Just v))

findValueIndex :: (a -> Bool) -> [a] -> Maybe (a, Int, NonEmpty a)
findValueIndex _ [] = Nothing
findValueIndex f x@(a : as) = case find (f . fst) (zip x [0..]) of
  Nothing     -> Nothing
  Just (b, i) -> Just (b, i, a :| as)

replace :: [a] -> Int -> a -> [a]
replace [] _ _ = []
replace (_:xs) 0 a = a : xs
replace (x:xs) n a =
  if n < 0
    then x : xs
    else x: replace xs (n-1) a

replace' :: NonEmpty a -> Int -> a -> NonEmpty a
replace' (_:|xs) 0 a = a :| xs
replace' (x:|xs) n a =
  if n < 0
    then x :| xs
    else x :| replace xs (n-1) a

loop' :: (t -> a) -> [t] -> [a]
loop' _ []       = []
loop' f (a : as) = f a : loop' f as

loop :: (a -> [a] -> NonEmpty a) -> NonEmpty a -> NonEmpty a
loop f (a :| as) = f a as

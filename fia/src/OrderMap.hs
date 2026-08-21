module OrderMap(
         Map, empty, fromList, toList, insert, delete, findWithDefault
       ) where

import Data.List(sortOn)
import qualified Data.Map as M

-- Map that allows to retrieve the keys in the order they were inserted.

data Map k v = MkOMap {
                 dictionary :: M.Map k (Integer, v),
                 nextIndex  :: Integer
               }

empty :: Map k v
empty = MkOMap M.empty 0

fromList :: Ord k => [(k, v)] -> Map k v
fromList list = 
  let keys    = map fst list
      values  = map snd list
      ivalues = zip [0..] values
   in MkOMap (M.fromList (zip keys ivalues)) (fromIntegral (length keys))

toList :: Map k v -> [(k, v)]
toList (MkOMap d _) = map (\ (k, (_, v)) -> (k, v))
                          (sortOn (\ (_, (i, v)) -> i)
                                  (M.toList d))

insert :: Ord k => k -> v -> Map k v -> Map k v
insert k v (MkOMap d i) = MkOMap (M.insert k (i, v) d) (i + 1)

delete :: Ord k => k -> Map k v -> Map k v
delete k (MkOMap d i) = MkOMap (M.delete k d) i

findWithDefault :: Ord k => v -> k -> Map k v -> v
findWithDefault v k (MkOMap d _) =
  case M.lookup k d of 
    Just (_, v') -> v'
    Nothing      -> v

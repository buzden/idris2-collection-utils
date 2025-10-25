module Data.List.Map

import Data.List
import Data.List.Set
import Data.SortedMap

public export
record ListMap k v where
  constructor MkListMap
  {auto eq : Eq k}
  kvList : List (k, v)

export
empty : Eq k => ListMap k v
empty = MkListMap empty

export
lookup : k -> ListMap k v -> Maybe v
lookup k (MkListMap kv) = lookup k kv

public export %inline
lookup' : ListMap k v -> k -> Maybe v
lookup' = flip lookup

export
insert : k -> v -> ListMap k v -> ListMap k v
insert k v (MkListMap kv) = MkListMap $ (k, v) :: kv

||| Inserts a key value pair into a map and merges duplicated values
||| with the given function.
export
insertWith : (v -> v -> v) -> k -> v -> ListMap k v -> ListMap k v
insertWith f k v xs =
  case lookup k xs of
    Just x  => insert k (f v x) xs
    Nothing => insert k v xs

public export %inline
insert' : ListMap k v -> (k, v) -> ListMap k v
insert' = flip $ uncurry insert

export
insertFrom : Foldable f => f (k, v) -> ListMap k v -> ListMap k v
insertFrom = flip $ foldl insert'

public export %inline
insertFrom' : Foldable f => ListMap k v -> f (k, v) -> ListMap k v
insertFrom' = flip insertFrom

||| Inserts any foldable of a key value pair into a map and merges duplicated
||| values with the given function.
export
insertFromWith : Foldable f => (v -> v -> v) -> f (k, v) -> ListMap k v -> ListMap k v
insertFromWith f = flip $ foldl $ flip $ uncurry $ insertWith f

export
singleton : Eq k => k -> v -> ListMap k v
singleton = MkListMap .: curry singleton

||| Updates existing value, if it is present, and does nothing otherwise
|||
||| The current implementation performs up to two traversals of the original map
export
updateExisting : (v -> v) -> k -> ListMap k v -> ListMap k v
updateExisting f k m = case lookup k m of
  Just v  => insert k (f v) m
  Nothing => m

public export %inline
updateExisting' : ListMap k v -> (v -> v) -> k -> ListMap k v
updateExisting' m f x = updateExisting f x m

public export %inline
fromList : Eq k => List (k, v) -> ListMap k v
fromList = MkListMap

||| Returns the keys from the underlying list of key–value pairs
||| without removing duplicates or normalising the order.
public export %inline
rawKeys : ListMap k v -> List k
rawKeys = map fst . kvList

||| Returns the values from the underlying list of key–value pairs
||| in their original order, without any normalisation.
public export %inline
rawValues : ListMap k v -> List v
rawValues = map snd . kvList

export
normalise : ListMap k v -> ListMap k v
normalise (MkListMap kv) = MkListMap $ nubBy ((==) `on` fst) kv

||| Gets the keys of the map.
export
keys : ListMap k v -> List k
keys = rawKeys . normalise

export
keySet : ListMap k v -> ListSet k
keySet m = MkListSet @{m.eq} $ rawKeys m

||| Gets the values of the map. Could contain duplicates.
public export %inline
values : ListMap k v -> List v
values = rawValues . normalise

export
implementation Functor (ListMap k) where
  map f (MkListMap kv) = MkListMap $ map (map f) kv

||| Merge two maps. When encountering duplicate keys, using a function to combine the values.
||| Uses the ordering of the first map given.
export
mergeWith : (v -> v -> v) -> ListMap k v -> ListMap k v -> ListMap k v
mergeWith f x y = insertFrom inserted x where
  inserted : List (k, v)
  inserted = do
    (k, v) <- kvList y
    let v' = (maybe id f $ lookup k x) v
    pure (k, v')

||| Merge two maps using the Semigroup (and by extension, Monoid) operation.
||| Uses mergeWith internally, so the ordering of the left map is kept.
export
merge : Semigroup v => ListMap k v -> ListMap k v -> ListMap k v
merge = mergeWith (<+>)

||| Left-biased merge, also keeps the equality specified by the left map.
export
mergeLeft : ListMap k v -> ListMap k v -> ListMap k v
mergeLeft (MkListMap @{eq} x) (MkListMap y) = MkListMap @{eq} $ x ++ y

public export %inline
toSortedMap : Ord k => ListMap k v -> SortedMap k v
toSortedMap = fromList . kvList

export
Semigroup v => Semigroup (ListMap k v) where
  (<+>) = merge

||| For `neutral <+> y`, y is rebuilt in `Eq k`, so this is not a "strict" Monoid.
||| However, semantically, it should be equal.
export
(Ord k, Semigroup v) => Monoid (ListMap k v) where
  neutral = empty

export
(Show k, Show v) => Show (ListMap k v) where
   show m = "fromList " ++ show (kvList m)

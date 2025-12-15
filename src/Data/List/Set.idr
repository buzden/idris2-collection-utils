module Data.List.Set

import Data.List
import Data.SortedSet

public export
record ListSet v where
  constructor MkListSet
  {auto eq : Eq v}
  rawValues : List v

export
empty : Eq v => ListSet v
empty = MkListSet empty

export
insert : v -> ListSet v -> ListSet v
insert v (MkListSet vs) = MkListSet $ v :: vs

public export %inline
insert' : ListSet v -> v -> ListSet v
insert' = flip insert

export
contains : v -> ListSet v -> Bool
contains v (MkListSet vs) = elem v vs

public export %inline
contains' : ListSet v -> v -> Bool
contains' = flip contains

export
singleton : Eq v => v -> ListSet v
singleton = MkListSet . singleton

export
fromList : Eq v => List v -> ListSet v
fromList = MkListSet

export
normalise : ListSet v -> ListSet v
normalise (MkListSet vs) = MkListSet $ nub vs

export  %inline
(.asList) : ListSet v -> List v
(.asList) = rawValues . normalise

||| Set union. Keeps the equality specified by the left set.
export
union : (x, y : ListSet v) -> ListSet v
union (MkListSet @{eq} xs) (MkListSet ys) = MkListSet @{eq} $ xs ++ ys

||| Set difference. Delete all elements in y from x.
||| Keeps the equality specified by the left set.
export
difference : (x, y : ListSet v) -> ListSet v
difference (MkListSet @{eq} xs) (MkListSet ys) = MkListSet @{eq} $ (xs \\ ys) @{eq}

||| Set symmetric difference. Uses the union of the differences.
export
symDifference : (x, y : ListSet v) -> ListSet v
symDifference x y = union (difference x y) (difference y x)

||| Set intersection. Implemented as the difference of the union and the symetric difference.
export
intersection : (x, y : ListSet v) -> ListSet v
intersection x y = difference x (difference x y)

public export %inline
toSortedSet : Ord v => ListSet v -> SortedSet v
toSortedSet = fromList . rawValues

export
Ord k => Semigroup (ListSet k) where
  (<+>) = union

export
Ord k => Monoid (ListSet k) where
  neutral = empty

export
Show k => Show (ListSet k) where
   show m = "fromList " ++ show (rawValues m)

export
Foldable ListSet where
  foldr f init = foldr f init . (.asList)
  foldl f init = foldl f init . (.asList)
  null $ MkListSet vs = null vs
  foldlM f init = foldlM f init . (.asList)
  toList = (.asList)

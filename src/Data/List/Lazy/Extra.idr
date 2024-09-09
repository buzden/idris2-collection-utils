module Data.List.Lazy.Extra

import public Data.List.Lazy

%default total

export
withIndex : LazyList a -> LazyList (Nat, a)
withIndex = go 0 where
  go : Nat -> LazyList a -> LazyList (Nat, a)
  go _ []      = []
  go n (x::xs) = (n, x) :: go (S n) xs

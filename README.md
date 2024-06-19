<!-- idris
module README

import Data.Collections.Analysis
import Data.Cozippable
import Data.List.Extra
import Data.List
import Data.List1
import Data.SortedSet
import Data.Zippable
-->

# Collection utilities

Additional utilities for standard Idris 2 collections

## Additional functions

There are a plenty of operations which from time to time we want to do with standard collections in a type-safe way.
For example, sometimes we may want to map a list or vector knowing an index of each element:

```idris
numerate : List String -> List String
numerate ss = mapI ss $ \i, s => "\{show i}. \{s}"
```

Or, say, to get all non-trivial pairs of values inside list with the other ones inside very this list.
And etc.
This library is for such a functions.

## Analysis of data stored as collections

Sometimes we want to analyse some data stored in a collection,
but this check uses collection just as a convenient way to represent particular data.

We can think of things like finding transitive closures of relations stored as lists,
or finding combinations or permutations of a list:

```idris
perms : Ord a => SortedSet a -> List1 $ List a
perms xs = permutations xs
```

## Cozippable interface

Well-known zippable interface works on common subpart of collections being zipped.
Say, when you have

```idris
oneList : List Nat
oneList = [0, 1, 2, 3, 4]

anotherList : List Nat
anotherList = [10, 20, 30]
```

when you `zip` them

```idris
ZippedLists : List Nat
ZippedLists = zipWith (+) oneList anotherList
```

the resulting list is the shortest one:

```idris
ZippedListsContents : ZippedLists = [10, 21, 32]
ZippedListsContents = Refl
```

But what if we would like to manage all the missing information too?
For that we have `cozip`:

```idris
CozippedLists : List Nat
CozippedLists = cozipWith (these' 100 200 (+)) oneList anotherList
```

and in this situation the resulting list has the length of the longest given one, giving us the ability to handle missing values
(in this particular example, by adding `100` and `200` for missing things in left and right list, correspondingly).
Look at the results:

```idris
CozippedListsContents : CozippedLists = [10, 21, 32, 203, 204]
CozippedListsContents = Refl
```

<!-- idris
module README

import Data.Collections.Analysis
import Data.List.Extra
import Data.List1
import Data.SortedSet
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

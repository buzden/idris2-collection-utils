module Data.Collections.Analysis

import Control.Monad.State

import Data.Fin.Map
import Data.Fin.Set
import Data.Fin.ToFin
import Data.List1
import Data.SortedSet.Extra
import Data.Vect.Dependent
import Data.Vect.Extra

import Syntax.IHateParens
import Syntax.Monad.Logic

%default total

--------------------------------
--- Transitive clojure stuff ---
--------------------------------

export covering
transitiveClosureM : Monad m => Eq a => (a -> m $ List a) -> List a -> m $ List a
transitiveClosureM f xs = tr xs xs where
  tr : (curr : List a) -> (new : List a) -> m $ List a
  tr curr [] = pure curr
  tr curr st = do
    next <- join <$> for st f
    let new = filter (not . (`elem` curr)) next
    tr (curr ++ new) new

export covering
holdsOnAnyInTrCl : Monad m => Eq a => (a -> Bool) -> (a -> m $ List a) -> List a -> m Bool
holdsOnAnyInTrCl prop f xs = pure (any prop xs) || tr xs xs where
  tr : (curr : List a) -> (new : List a) -> m Bool
  tr curr [] = pure False
  tr curr st = do
    next <- join <$> for st f
    let new = filter (not . (`elem` curr)) next
    pure (any prop new) || tr (curr ++ new) new

---------------------------------------------------
--- Splitting and rejoining `Either`'ed `Vect`s ---
---------------------------------------------------

-- Returns also original positions of `Left`'s
export
partitionEithersPos : {n : _} -> Vect n (Either a b) -> (List a, List b, FinSet n)
partitionEithersPos = map @{Compose} fromList . p where
  p : forall n. Vect n (Either a b) -> (List a, List b, List $ Fin n)
  p []        = ([], [], empty)
  p (ab::abs) = let (as, bs, lefts) = p abs
                    lefts = FS <$> lefts
                in case ab of
                  Left  a => (a::as,    bs, FZ::lefts)
                  Right b => (   as, b::bs,     lefts)

export
joinEithersPos : (as : List a) -> (bs : List b) -> FinSet (as.length + bs.length) -> Maybe $ Vect (as.length + bs.length) $ Either a b
joinEithersPos as bs lefts =
  evalState (as, bs) $ for @{Compose} range $ \idx => if contains idx lefts
    then do
      (x::as, bs) <- get
        | ([], _) => pure $ Nothing
      put (as, bs)
      pure $ Just $ Left x
    else do
      (as, x::bs) <- get
        | (_, []) => pure $ Nothing
      put (as, bs)
      pure $ Just $ Right x

-------------------------------
--- Manupulations with sets ---
-------------------------------

export
disjointDepSets : {n : _} -> (rawDeps : DVect n $ FinSet . Fin.finToNat) -> (givs : FinSet n) -> List $ FinSet n
disjointDepSets rawDeps givs = do

  -- For each argument calculate the minimal index of its dependency (itself, if no dependencies)
  let minDeps = flip mapPreI rawDeps $ \i, pre => maybe last FS . leftMost . (`difference` fit givs)

  -- Get rid of dependent vector, weaken indices bounds
  let minDeps = downmap (weakenToSuper {i=FS _}) minDeps

  -- Reverse the map, i.e. for each minimal index get the set of arguments that depend on it
  let minDeps : FinMap .| S n .| FinSet n :=
    concatMap .| uncurry singleton .| mapSnd singleton <$> toListI minDeps

  -- Acquire a list of disjoint sets, which in each set all args dependent somehow, but args from different susets are independent
  Prelude.toList minDeps

---------------------
--- Combinatorics ---
---------------------

public export
combinations : Vect n (List1 a) -> List1 (Vect n a)
combinations l = map (rewrite plusZeroRightNeutral n in reverse) $ go l $ Nil:::[] where
  go : Vect m (List1 a) -> List1 (Vect k a) -> List1 (Vect (m + k) a)
  go           Nil       rss = rss
  go {m = S m} (xs::xss) rss = rewrite plusSuccRightSucc m k in
    go xss $ join $ map (\x => map (\rs => x :: rs) rss) xs

export
permutations : Ord a => SortedSet a -> List1 $ List a
permutations s = case fromList s.asList of
  Nothing => pure []
  Just ss => do
    e  <- ss
    es <- permutations $ assert_smaller s $ delete e s
    pure $ e :: es

export
permutationsFin : {n : _} -> FinSet n -> List1 $ List $ Fin n
permutationsFin s = case fromList s.asList of
  Nothing => pure []
  Just ss => do
    e  <- ss
    es <- permutationsFin $ assert_smaller s $ delete e s
    pure $ e :: es

public export
permutations' : Ord a => SortedSet a -> List $ List a
permutations' = forget . permutations

export
indepPermutations : {n : _} -> (independencyGroups : List $ FinSet n) -> FinSet n -> List1 $ List $ Fin n
indepPermutations groups s = map concat $ for groups $ permutationsFin . intersection s

public export %inline
indepPermutations' : {n : _} -> (independencyGroups : List $ FinSet n) -> FinSet n -> List $ List $ Fin n
indepPermutations' = forget .: indepPermutations

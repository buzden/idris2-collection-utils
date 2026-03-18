module Spec

import Data.List
import Data.List.Set
import Data.SortedSet
import Data.Vect

import Hedgehog

Value : Type
Value = Fin 5

value : Gen Value
value = fin $ linearFin _

genList : Gen $ List Value
genList = list (linear 0 100) value

genListSet : Gen $ ListSet Value
genListSet = fromList <$> genList

genSets : Gen (ListSet Value, SortedSet Value)
genSets = do
  pairs <- genList
  pure (fromList pairs, fromList pairs)

equals : ListSet Value -> SortedSet Value -> PropertyT ()
equals ls ss = do
  x <- forAll value
  assert $
    contains x ls == contains x ss

prop_empty : Property
prop_empty = property $ do
  empty `equals`
  empty

prop_singleton : Property
prop_singleton = property $ do
  x <- forAll value
  singleton x `equals`
  singleton x

prop_insert : Property
prop_insert = property $ do
  (ls, ss) <- forAll genSets
  x <- forAll value
  insert x ls `equals`
  insert x ss

prop_union : Property
prop_union = property $ do
  (ls1, ss1) <- forAll genSets
  (ls2, ss2) <- forAll genSets
  x <- forAll value
  union ls1 ls2 `equals`
  union ss1 ss2

prop_intersection : Property
prop_intersection = property $ do
  (ls1, ss1) <- forAll genSets
  (ls2, ss2) <- forAll genSets
  x <- forAll value
  intersection ls1 ls2 `equals`
  intersection ss1 ss2

prop_delete : Property
prop_delete = property $ do
  (ls, ss) <- forAll genSets
  x <- forAll value
  delete x ls `equals`
  delete x ss

prop_difference : Property
prop_difference = property $ do
  (ls1, ss1) <- forAll genSets
  (ls2, ss2) <- forAll genSets
  x <- forAll value
  difference ls1 ls2 `equals`
  difference ss1 ss2

prop_symDifference : Property
prop_symDifference = property $ do
  (ls1, ss1) <- forAll genSets
  (ls2, ss2) <- forAll genSets
  x <- forAll value
  symDifference ls1 ls2 `equals`
  symDifference ss1 ss2

prop_fromList : Property
prop_fromList = property $ do
  ls <- forAll genList
  fromList ls `equals`
  fromList ls

prop_toList : Property
prop_toList = property $ do
  ls <- forAll genListSet
  x <- forAll value
  assert $
    contains x ls == elem x (toList ls)

prop_toSortedSet : Property
prop_toSortedSet = property $ do
  ls <- forAll genListSet
  x <- forAll value
  assert $
    contains x ls == contains x (toSortedSet ls)

prop_normalise_idempotent : Property
prop_normalise_idempotent = property $ do
  ls <- forAll genListSet
  assert $
    toList (normalise (normalise ls)) == toList (normalise ls)

prop_normalise_equal : Property
prop_normalise_equal = property $ do
  ls <- forAll genListSet
  x <- forAll value
  assert $
    contains x (normalise ls) == contains x ls

prop_semigroup : Property
prop_semigroup = property $ do
  (ls1, ss1) <- forAll genSets
  (ls2, ss2) <- forAll genSets
  ls1 <+> ls2 `equals`
  ss1 <+> ss2

prop_monoid : Property
prop_monoid = property $ do
  neutral `equals`
  neutral

prop_null : Property
prop_null = property $ do
  (ls, ss) <- forAll genSets
  assert $
    null ls == null ss

prop_foldr : Property
prop_foldr = property $ do
  ls <- forAll genListSet
  assert $
    foldr (::) [] ls == toList ls

prop_foldl : Property
prop_foldl = property $ do
  ls <- forAll genListSet
  assert $
    foldl (flip (::)) [] ls == reverse (toList ls)

prop_foldlM : Property
prop_foldlM = property $ do
  ls <- forAll genListSet
  assert $
    foldlM (Just .: flip (::)) [] ls == Just (reverse $ toList ls)

props : Group
props =
  MkGroup "ListSet"
    [ ("empty", prop_empty)
    , ("singleton", prop_singleton)
    , ("insert", prop_insert)
    , ("union", prop_union)
    , ("fromList", prop_fromList)
    , ("delete", prop_delete)
    , ("difference", prop_difference)
    , ("symDifference", prop_symDifference)
    , ("intersection", prop_intersection)
    , ("toSortedSet", prop_toSortedSet)
    , ("semigroup", prop_semigroup)
    , ("monoid", prop_monoid)
    , ("null", prop_null)
    , ("toList", prop_toList)
    , ("foldr", prop_foldr)
    , ("foldl", prop_foldl)
    , ("foldlM", prop_foldlM)
    , ("normalise idempotent", prop_normalise_idempotent)
    , ("normalise equal", prop_normalise_equal)
    ]

main : IO ()
main = test [props]
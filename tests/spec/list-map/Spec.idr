module Spec

import Data.List
import Data.List.Map
import Data.List.Set
import Data.SortedMap
import Data.SortedMap.Extra
import Data.Vect

import Hedgehog

Key : Type
Key = Fin 5

Value : Type
Value = Integer

Semigroup Value where
  (<+>) = (+)

key : Gen Key
key = fin $ linearFin _

value : Gen Value
value = cast <$> anyInt64

genAssociatedList : Gen (List (Key, Value))
genAssociatedList = list (linear 0 100) [| (key, value) |]

genListMap : Gen (ListMap Key Value)
genListMap = fromList <$> genAssociatedList

genMaps : Gen (ListMap Key Value, SortedMap Key Value)
genMaps = do
  pairs <- genAssociatedList
  pure (fromList pairs, fromList pairs)

equals : ListMap Key Value -> SortedMap Key Value -> PropertyT ()
equals lm sm = do
  k <- forAll key
  assert $
    lookup k lm == lookup k sm

prop_empty : Property
prop_empty = property $
  empty `equals`
  empty

prop_singleton : Property
prop_singleton = property $ do
  k <- forAll key
  v <- forAll value
  singleton k v `equals`
  singleton k v

prop_insert : Property
prop_insert = property $ do
  (lm, sm) <- forAll genMaps
  k <- forAll key
  v <- forAll value
  insert k v lm `equals`
  insert k v sm

prop_insertWith : Property
prop_insertWith = property $ do
  (lm, sm) <- forAll genMaps
  k <- forAll key
  v <- forAll value
  insertWith (-) k v lm `equals`
  insertWith (-) k v sm

prop_insertFrom : Property
prop_insertFrom = property $ do
  (lm, sm) <- forAll genMaps
  pairs <- forAll genAssociatedList
  k <- forAll key
  insertFrom pairs lm `equals`
  insertFrom pairs sm

prop_insertFromWith : Property
prop_insertFromWith = property $ do
  (lm, sm) <- forAll genMaps
  pairs <- forAll genAssociatedList
  k <- forAll key
  insertFromWith (-) pairs lm `equals`
  insertFromWith (-) pairs sm

prop_updateExisting : Property
prop_updateExisting = property $ do
  (lm, sm) <- forAll genMaps
  k <- forAll key
  v <- forAll value
  updateExisting (+1) k lm `equals`
  updateExisting (+1) k sm

prop_keys : Property
prop_keys = property $ do
  m <- forAll genListMap
  k <- forAll key
  assert $
    isJust (lookup k m) == elem k (keys m)

prop_keySet : Property
prop_keySet = property $ do
  m <- forAll genListMap
  k <- forAll key
  assert $
    isJust (lookup k m) == contains k (keySet m)

prop_values : Property
prop_values = property $ do
  (lm, sm) <- forAll genMaps
  k <- forAll key
  assert $
    sort (values lm) == sort (values sm)

prop_mapWithKey : Property
prop_mapWithKey = property $ do
  (lm, sm) <- forAll genMaps
  k <- forAll key
  mapWithKey f lm `equals`
  mapWithKey f sm
  where
    f : Key -> Value -> Value
    f k v = cast k + v

prop_mergeWith : Property
prop_mergeWith = property $ do
  (lm1, sm1) <- forAll genMaps
  (lm2, sm2) <- forAll genMaps
  k <- forAll key
  mergeWith (+) lm1 lm2 `equals`
  mergeWith (+) sm1 sm2

prop_merge : Property
prop_merge = property $ do
  (lm1, sm1) <- forAll genMaps
  (lm2, sm2) <- forAll genMaps
  k <- forAll key
  merge lm1 lm2 `equals`
  merge sm1 sm2

prop_mergeWithLeft : Property
prop_mergeWithLeft = property $ do
  (lm1, sm1) <- forAll genMaps
  (lm2, sm2) <- forAll genMaps
  k <- forAll key
  mergeWith const lm1 lm2 `equals`
  mergeWith const sm1 sm2

prop_fromList : Property
prop_fromList = property $ do
  pairs <- forAll genAssociatedList
  k <- forAll key
  fromList pairs `equals`
  fromList pairs

prop_toList : Property
prop_toList = property $ do
  ls <- forAll genListMap
  x <- forAll value
  assert $
    toList ls == values ls

prop_toSortedMap : Property
prop_toSortedMap = property $ do
  m <- forAll genListMap
  k <- forAll key
  assert $
    lookup k m == lookup k (toSortedMap m)

prop_semigroup : Property
prop_semigroup = property $ do
  (lm1, sm1) <- forAll genMaps
  (lm2, sm2) <- forAll genMaps
  k <- forAll key
  lm1 <+> lm2 `equals`
  sm1 <+> sm2

prop_monoid : Property
prop_monoid = property $ do
  neutral `equals`
  neutral

prop_null : Property
prop_null = property $ do
  (lm, sm) <- forAll genMaps
  assert $
    null lm == null sm

prop_foldr : Property
prop_foldr = property $ do
  ls <- forAll genListMap
  assert $
    foldr (::) [] ls == toList ls

prop_foldl : Property
prop_foldl = property $ do
  ls <- forAll genListMap
  assert $
    foldl (flip (::)) [] ls == reverse (toList ls)

prop_foldlM : Property
prop_foldlM = property $ do
  ls <- forAll genListMap
  assert $
    foldlM (Just .: flip (::)) [] ls == Just (reverse $ toList ls)

prop_traverse : Property
prop_traverse = property $ do
  (lm, sm) <- forAll genMaps
  k <- forAll key
  let Just lm' = traverse Just lm
    | Nothing => failure
  let Just sm' = traverse Just (toSortedMap lm)
    | Nothing => failure
  lm' `equals`
  sm'

prop_normalise_idempotent : Property
prop_normalise_idempotent = property $ do
  xs <- forAll genListMap
  assert $
    kvList (normalise (normalise xs)) == kvList (normalise xs)

prop_normalise_equal : Property
prop_normalise_equal = property $ do
  xs <- forAll genListMap
  k <- forAll key
  assert $
    lookup k (normalise xs) == lookup k xs

props : Group
props = MkGroup "ListMap"
    [ ("empty", prop_empty)
    , ("singleton", prop_singleton)
    , ("insert", prop_insert)
    , ("insertWith", prop_insertWith)
    , ("insertFrom", prop_insertFrom)
    , ("insertFromWith", prop_insertFromWith)
    , ("updateExisting", prop_updateExisting)
    , ("fromList", prop_fromList)
    , ("keys", prop_keys)
    , ("keySet", prop_keySet)
    , ("values", prop_values)
    , ("mapWithKey", prop_mapWithKey)
    , ("mergeWith", prop_mergeWith)
    , ("merge", prop_merge)
    , ("mergeWithLeft", prop_mergeWithLeft)
    , ("toSortedMap", prop_toSortedMap)
    , ("semigroup", prop_semigroup)
    , ("monoid", prop_monoid)
    , ("null", prop_null)
    , ("toList", prop_toList)
    , ("foldr", prop_foldr)
    , ("foldl", prop_foldl)
    , ("foldlM", prop_foldlM)
    , ("traverse", prop_traverse)
    , ("normalise_idempotent", prop_normalise_idempotent)
    , ("normalise_equal", prop_normalise_equal)
    ]

main : IO ()
main = test [props]
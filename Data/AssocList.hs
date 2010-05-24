module Data.AssocList where

  import Data.List
  import Data.List.Utils

  -- Library of Association List functions and utilities.
  -- An Association List is a list of key,value pairs, (todo: where uniqueness of the key is enforced.)
  -- This is mainly an extension of Data.List.Utils, provided in the missingh libraries



  -- Currently no new content

  -- Aliases

  add :: Eq  key => [(key, elt)] -> key -> elt -> [(key, elt)]
  add = addToAL

  del :: Eq  key => [(key, a)] -> key -> [(key, a)]
  del = delFromAL

  swap :: (Eq  key, Eq  val) => [(key, val)] -> [(val, [key])]
  swap = flipAL

  keys :: [(key, a)] -> [key]
  keys = keysAL

  values :: [(a, value)] -> [value]
  values = valuesAL

  hasKey :: Eq a => a -> [(a, b)] -> Bool
  hasKey = hasKeyAL
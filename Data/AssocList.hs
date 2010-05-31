module Data.AssocList where

  import Data.List
  import Data.List.Utils
  import Control.Exception
  import Data.Dynamic

  -- | Library of Association List functions and utilities.
  -- An Association List is a list of key,value pairs
  -- (todo: enforce uniqueness at every step)
  -- This is mainly an extension of Data.List.Utils, provided in "missingh"

  type AssocList a b = [(a,b)]

  -- | Runtime exception for when an entry is not found
  notFound = throw $ ErrorCall "Nonexistant Key"

  -- | Add, removing any existing pair with the same key
  add :: Eq  key => [(key, elt)] -> key -> elt -> [(key, elt)]
  add = addToAL

  -- | Remove all pairs with the key
  del :: Eq  key => [(key, a)] -> key -> [(key, a)]
  del = delFromAL

  -- | Swap out keys and values
  swap :: (Eq  key, Eq  val) => [(key, val)] -> [(val, [key])]
  swap = flipAL

  keys :: [(key, a)] -> [key]
  keys = keysAL

  -- | Get the values out
  values :: [(a, value)] -> [value]
  values = valuesAL

  -- | Whether the association list has the key
  hasKey :: Eq a => a -> [(a, b)] -> Bool
  hasKey = hasKeyAL

  -- | Partial function for lookup. Throws notFound if not found
  unsafeFetch :: Eq a => a -> [(a, b)] -> b
  unsafeFetch k w = case lookup k w of Just v -> v
                                       Nothing -> notFound
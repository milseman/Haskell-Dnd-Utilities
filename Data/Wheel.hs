module Data.Wheel where
  import Data.List

  -- | Library providing the wheel data structure, and many useful functions
  -- A Wheel is a list that rotates around, and elements can be inserted,
  -- updated, removed, and events can occur when an item passes by (such
  -- as a counter updated, etc).

  -- | A Wheel is just a type alias for a list, so useful list functions can be used,
  -- e.g. filter.

  type Wheel a = [a]
  type Rotation a = Wheel a -> Wheel a
  type Predicate a = a -> Bool
  type Processor a = a -> a

  data Position = Before | After

  -- | Top of the wheel
  top :: Wheel a -> a
  top = head

  -- | Rotate forward
  forward :: Rotation a
  forward (x:xs) = xs ++ [x]
  forward x = x

  -- | Rotate backward
  backward :: Rotation a
  backward (x:xs) = last (x:xs) : init (x:xs)
  backward x = x

  -- | Rotate until the top element satisfies predicate.
  -- If no element satisfies, no rotation occurs.
  -- rotM: rotation method to use until p satisfies
  rotateUntil :: Rotation a -> Wheel a -> Predicate a -> Wheel a
  rotateUntil rotM w p = checkFirst w p $ safeRotateUntil rotM w p
    where safeRotateUntil rotM w p = if p (top w) then w
                                            else safeRotateUntil rotM (rotM w) p

  -- | RotateUntil specialized for forward
  forwardUntil :: Wheel a -> Predicate a -> Wheel a
  forwardUntil = rotateUntil forward

  -- | RotateUntil specialized for backward
  backwardUntil :: Wheel a -> Predicate a -> Wheel a
  backwardUntil = rotateUntil backward

  -- | Insert an item at given position relative to the first element to satisfy
  -- the predicate.
  -- If no element satisfies the predicate, stick at end
  insertAt :: Wheel a -> Predicate a -> a -> Position -> Wheel a
  insertAt [] p x pos = [x]
  insertAt (e:es) p x Before = if p e then x : e : es else e : insertAt es p x Before
  insertAt (e:es) p x After = if p e then e : x : es else e : insertAt es p x After

  -- | Move the top element to given position relative to the first element to satisfy
  -- the predicate
  -- If no element satisfies the predicate, stick at end
  move :: Wheel a -> Predicate a -> Position -> Wheel a
  move (x:xs) p pos = insertAt xs p x pos
  move [] _ _ = []

  -- | Process an element and advance.
  -- The supplied funtion takes the top of the wheel, and returns a new element
  -- to replace it, then the wheel is rotated forward
  process :: Wheel a -> Processor a -> Wheel a
  process (x:xs) f = forward $ f x : xs
  process [] f = []

  -- | Replace the first entry matching the predicate with the new entry
  -- If no entries match, the original wheel is returned
  replace :: Wheel a -> a -> (a -> Bool) -> Wheel a
  replace w new p = case break p w of (left, r:rs) -> left ++ (new:rs)
                                      (left, []) -> left


  -- Internal helper: see if no one in the wheel satisfies
  none :: Wheel a -> Predicate a -> Bool
  none w p = not $ any p w

  -- Internal helper: Control flow construct that returns argument wheel when
  -- none satisfy, else continues to perform operation.
  -- Relies on laziness
  checkFirst :: Wheel a -> Predicate a -> Wheel a -> Wheel a
  checkFirst w p operation = if none w p then w else operation

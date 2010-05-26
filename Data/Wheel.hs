module Data.Wheel where
  import Data.List

  -- Library providing the wheel data structure, and many useful functions
  -- A Wheel is a list that rotates around, and elements can be inserted,
  -- updated, removed, and events can occur when an item passes by (such
  -- as a counter updated, etc).

  -- A Wheel is just a type alias for a list, so useful list functions can be used,
  -- e.g. filter.

  type Wheel a = [a]
  type Rotation a = Wheel a -> Wheel a
  type Predicate a = a -> Bool
  type Processor a = a -> a

  -- Top of the wheel
  top :: Wheel a -> a
  top = head

  -- Rotate forward
  forward :: Rotation a
  forward (x:xs) = xs ++ [x]
  forward x = x

  -- Rotate backward
  backward :: Rotation a
  backward (x:xs) = last (x:xs) : init (x:xs)
  backward x = x

  -- Rotate until the top element satisfies predicate.
  -- If no element satisfies, no rotation occurs.
  -- rotM: rotation method to use until p satisfies
  rotateUntil :: Rotation a -> Wheel a -> Predicate a -> Wheel a
  rotateUntil rotM w p = checkFirst w p $ safeRotateUntil rotM w p
    where safeRotateUntil rotM w p = if p (top w) then w
                                            else safeRotateUntil rotM (rotM w) p

  -- RotateUntil specialized for forward
  forwardUntil :: Wheel a -> Predicate a -> Wheel a
  forwardUntil = rotateUntil forward

  -- RotateUntil specialized for backward
  backwardUntil :: Wheel a -> Predicate a -> Wheel a
  backwardUntil = rotateUntil backward

  -- Insert an item right before the first element to satisfy the predicate.
  -- If no element satisfies the predicate, stick at end
  insertBefore :: Wheel a -> Predicate a -> a -> Wheel a
  insertBefore [] p x = [x]
  insertBefore (e:es) p x = if p e then x : e : es else e : insertBefore es p x


  -- Insert an item right after the first element to satisfy the predicate.
  -- If no element satisfies the predicate, stick at end
  insertAfter :: Wheel a -> Predicate a -> a -> Wheel a
  insertAfter [] p x = [x]
  insertAfter (e:es) p x = if p e then e : x : es else e : insertAfter es p x

  -- Move the top element to before the first element to satisfy the predicate
  -- If no element satisfies the predicate, stick at end
  moveBefore :: Wheel a -> Predicate a -> Wheel a
  moveBefore (x:xs) p = insertBefore xs p x
  moveBefore [] p = []

  -- Move the top element to after the first element to satisfy the predicate
  -- If no element satisfies the predicate, stick at end
  moveAfter :: Wheel a -> Predicate a -> Wheel a
  moveAfter (x:xs) p = insertAfter xs p x
  moveAfter [] p = []

  -- Process an element and advance.
  -- The supplied funtion takes the top of the wheel, and returns a new element
  -- to replace it, then the wheel is rotated forward
  process :: Wheel a -> Processor a -> Wheel a
  process (x:xs) f = forward $ f x : xs
  process [] f = []


  -- Internal helper: see if no one in the wheel satisfies
  none :: Wheel a -> Predicate a -> Bool
  none w p = not $ any p w

  -- Internal helper: Control flow construct that returns argument wheel when
  -- none satisfy, else continues to perform operation.
  -- Relies on laziness
  checkFirst :: Wheel a -> Predicate a -> Wheel a -> Wheel a
  checkFirst w p operation = if none w p then w else operation

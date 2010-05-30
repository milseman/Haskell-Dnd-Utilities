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
  rotateUntil :: Rotation a -> Predicate a -> Wheel a -> Wheel a
  rotateUntil rotM p w = checkFirst p w $ safeRotateUntil rotM p w
    where safeRotateUntil rotM p w = if p (top w) then w
                                            else safeRotateUntil rotM p (rotM w)

  -- | RotateUntil specialized for forward
  forwardUntil :: Predicate a -> Wheel a -> Wheel a
  forwardUntil = rotateUntil forward

  -- | RotateUntil specialized for backward
  backwardUntil :: Predicate a -> Wheel a -> Wheel a
  backwardUntil = rotateUntil backward

  -- | Insert an item at given position relative to the first element to satisfy
  -- the predicate.
  -- If no element satisfies the predicate, stick at end
  insertAt :: Predicate a -> a -> Position -> Wheel a -> Wheel a
  insertAt p x pos [] = [x]
  insertAt p x Before (e:es) = if p e then x : e : es else e : insertAt p x Before es
  insertAt p x After (e:es)  = if p e then e : x : es else e : insertAt p x After es

  -- | Move the top element to given position relative to the first element to satisfy
  -- the predicate
  -- If no element satisfies the predicate, stick at end
  move :: Predicate a -> Position -> Wheel a -> Wheel a
  move p pos (x:xs) = insertAt p x pos xs
  move _ _ [] = []

  -- | Process an element and advance.
  -- The supplied funtion takes the top of the wheel, and returns a new element
  -- to replace it, then the wheel is rotated forward
  process :: Processor a -> Wheel a -> Wheel a
  process f (x:xs) = forward $ f x : xs
  process f [] = []

  -- | Replace the first entry matching the predicate with the new entry
  -- If no entries match, the original wheel is returned
  replace :: a -> (a -> Bool) -> Wheel a -> Wheel a
  replace new p w = case break p w of (left, r:rs) -> left ++ (new:rs)
                                      (left, []) -> left

  -- Internal helper: see if no one in the wheel satisfies
  none :: Predicate a -> Wheel a -> Bool
  none p w = not $ any p w

  -- Internal helper: Control flow construct that returns argument wheel when
  -- none satisfy, else continues to perform operation.
  -- Relies on laziness
  checkFirst :: Predicate a -> Wheel a -> Wheel a -> Wheel a
  checkFirst p operation w = if none p w then w else operation

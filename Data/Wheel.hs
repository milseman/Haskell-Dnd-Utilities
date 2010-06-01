module Data.Wheel where
  import Data.List
  import Control.Exception

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

  -- | Runtime exception for when no entry matches a given predicate
  unsatisfiedPredicate = throw $ ErrorCall "Unsatisfied Predicate"

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
  rotateUntil rotM p w = checkWheel p w $ safeRotateUntil rotM p w
    where safeRotateUntil rotM p w = if p (top w) then w
                                            else safeRotateUntil rotM p (rotM w)

  -- | RotateUntil specialized for forward
  forwardUntil :: Predicate a -> Wheel a -> Wheel a
  forwardUntil = rotateUntil forward

  -- | RotateUntil specialized for backward
  backwardUntil :: Predicate a -> Wheel a -> Wheel a
  backwardUntil = rotateUntil backward

  -- | Insert an item at given position relative to the first element to satisfy
  -- the predicate. Throws unsatisfiedPredicate
  insertAt :: Predicate a -> a -> Position -> Wheel a -> Wheel a
  insertAt p x pos w = checkWheel p w $ insertW p x pos w
    where insertW p x Before (e:es) = if p e then x : e : es
                                     else e : insertW p x Before es
          insertW p x After (e:es)  = if p e then e : x : es
                                      else e : insertW p x After es

  -- | As insertAt, but if predicate is never satisfied, insert at end
  insertAtElseEnd :: Predicate a -> a -> Position -> Wheel a -> Wheel a
  insertAtElseEnd p x pos w = if any p w then insertAt p x pos w
                                   else (w ++ [x])

  -- | Move the top element to given position relative to the first element to satisfy
  -- the predicate. Throws unsatisfiedPredicate
  moveTo :: Predicate a -> Position -> Wheel a -> Wheel a
  moveTo p pos w = checkWheel p w $ insertAt p (head w) pos (tail w)

  -- | Process an element and advance.
  -- The supplied funtion takes the top of the wheel, and returns a new element
  -- to replace it, then the wheel is rotated forward
  process :: Processor a -> Wheel a -> Wheel a
  process f (x:xs) = forward $ f x : xs
  process f [] = []

  -- | Replace the first entry matching the predicate with the new entry.
  -- Throws unsatisfiedPredicate
  replace :: a -> (a -> Bool) -> Wheel a -> Wheel a
  replace new p w =
    checkWheel p w $ case break p w of (left, r:rs) -> left ++ (new:rs)

  -- Internal helper: Make sure an element exists that satisfies the predicate
  -- If none do, throw "Unsatisfied predicate"
  -- Takes advantage of laziness to implement control flow what wont evaluate
  -- the second argument if p is never satisfied
  checkWheel :: Predicate a -> Wheel a -> Wheel a -> Wheel a
  checkWheel p w safeW | any p w = safeW
  checkWheel _ _ _ = unsatisfiedPredicate

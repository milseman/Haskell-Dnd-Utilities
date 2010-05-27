module Dnd.DM.CombatTracker where
  import Data.AssocList
  import Data.Wheel

  import Data.List

  import qualified Text.PrettyPrint.HughesPJ as P
  import Text.PrettyPrint.HughesPJ (($+$), (<>), ($$), text, nest)


  type Agent = String

  data Info = String String | Int Int
              deriving (Show, Eq)

  type Entry = (Agent, AssocList String Info)
  type Character = Entry
  type Effect = Entry
  type Marker = Entry

  type CombatWheel = Wheel Entry

  -- Combat wheel
  combat :: CombatWheel
  combat = [topOfRound]

  -- Entry for the top of the round
  topOfRound :: Marker
  topOfRound =
    ( "Top Of Round", [ ("Round Number", Int 1)
                      , ("Superfluous Stuff", String "Inferiorfluous Stuff")
                      ]
    )

  -- Find the current round number of combat. Undefined in the absence of a top
  -- of round marker
  roundNumber :: CombatWheel -> Int
  roundNumber w = case unsafeFetch "Round Number" (unsafeFetch "Top Of Round" w) of
                    Int i -> i

  -- Create a character given name and initiative
  createCharacter :: String -> Int -> Character
  createCharacter s i = (s, [("Initiative", Int i)])

  -- Create an effect given name and duration
  createEffect :: String -> Int -> Effect
  createEffect s i = (s, [("Duration", Int i)])

  -- Add a character to a combat wheel
  addCharacter :: CombatWheel -> Character -> CombatWheel
  addCharacter w c = insertBefore w (isBefore c) c

  -- List version of above
  addCharacters :: CombatWheel -> [Character] -> CombatWheel
  addCharacters = foldl addCharacter

  -- change me: figure out how to precisely do this
  -- Add an effect, such as an ongoing spell, to the slot right after the top
  -- of the wheel.
  addEffect :: CombatWheel -> Effect -> CombatWheel
  addEffect (e:es) effect = e : effect : es
  addEffect [] effect = [effect]


  -- Makes the top most entry delay until after the provided character's turn
  -- Will delay until the end of the wheel if given character's name does not
  -- exist in combat wheel (i.e. a single rotation without update)
  -- Note: Doesn't actually update the initiative value
  delay :: CombatWheel -> String -> CombatWheel
  delay w s = moveAfter w (\c -> fst c == s)


  -- Advance the round, processing durations, round numbers, etc
  advance :: CombatWheel -> CombatWheel
  advance (e:es) | isOver e = es
  advance w = process w updateEntry 



  -- | Pretty Printing

  -- Doc for CombatWheels
  combatDoc :: CombatWheel -> P.Doc
  combatDoc w =
    text "Combat: " <> text ("(round " ++ show (roundNumber w) ++ ")")
      $$ nest 4 (P.vcat (map entryDoc w))

  -- Doc for Entries
  entryDoc :: Entry -> P.Doc
  entryDoc (s, al) = text s <> text (": ") $$ nest 25 (alDoc al)

  -- Doc for Association Lists
  alDoc :: (Show a, Show b) => AssocList a b -> P.Doc
  alDoc ((a,b):al) = P.vcat (text "[ " <> innard a b : rest al) <> text " ]"
    where innard a b = text $ show a ++ " => " ++ show b
          rest al = map (\(a,b) -> text ", " <> innard a b) al

  pp :: CombatWheel -> IO()
  pp w = putStrLn . P.render $ combatDoc w

  -- | Utility functions

  -- Initiative of a character
  initOf :: Character -> Maybe Info
  initOf = lookup "Initiative" . snd

  {- Predicate telling whether the first argument comes before the second. Attempts
     to look for the field Initiative in the second argument, at which points it
     compares. If Initiative is not present, or is non-integer, then false is
     returned.
     Undefined if the first argument has no integer Initiative.
   -}
  isBefore :: Character -> Character -> Bool
  isBefore char char2 =
    case (initOf char, initOf char2) of
      (Just (Int left), Just (Int right)) -> left > right
      (Just (Int i), _) -> False

  -- Update the entry, i.e. decrement durations, increase turn counts, etc.
  updateEntry :: Entry -> Entry
  updateEntry (a, al) = (a, handle "Duration" (handle "Round Number" al))
    where handle "Duration" al = case lookup "Duration" al of
                                   Just (Int i) -> incr "Duration" (i-1)
                                   Nothing -> al
          handle "Round Number" al = case lookup "Round Number" al of
                                       Just (Int i) -> incr "Round Number" (i+1)
                                       Nothing -> al
          incr s i = add al s (Int i)

  -- Predicate saying whether the current effect is over (i.e. Duration 0)
  -- If Duration is missing, or non-zero, then false. Else true
  isOver :: Entry -> Bool
  isOver (a, al) = case lookup "Duration" al of
                     Just (Int 0) -> True
                     _ -> False

  -- More to come


  -- Some test dudes
  dummy1 = createCharacter "Dummy 1" 4
  dummy2 = createCharacter "Dummy 2" 1
  dummy3 = createCharacter "Dummy 3" 14
  dummy4 = createCharacter "Dummy 4" 20

  effect1 = ("Magical Effect Foo", [("Duration", Int 4)])

  combat1 = addCharacters combat [dummy3, dummy2, dummy1]
  combat2 = addEffect combat1 effect1

  example1 = combatDoc combat1
  example2 = combatDoc combat2

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
  topOfRound = ("Top Of Round", [ ("Round Number", Int 0)
                                , ("Initiative", String "Top")
                                ])

  -- Find the current round number of combat. Undefined in the absence of a top
  -- of round marker
  roundNumber :: CombatWheel -> Int
  roundNumber w = case unsafeFetch "Round Number" (unsafeFetch "Top Of Round" w) of
                    Int i -> i

  -- Create a character given name and initiative
  createCharacter :: String -> Int -> Character
  createCharacter s i = (s, [("Initiative", Int i)])


  -- Add a character to a combat wheel
  addCharacter :: CombatWheel -> Character -> CombatWheel
  addCharacter w c = insertBefore w (isBefore c) c

  -- List version of above
  addCharacters :: CombatWheel -> [Character] -> CombatWheel
  addCharacters = foldl addCharacter

  -- Add an effect, such as an ongoing spell, to the top of the wheel
  addEffect :: CombatWheel -> Effect -> CombatWheel
  addEffect = flip (:)

  -- Makes the top most entry delay until after the provided character's turn
  -- Will delay until the end of the wheel if given character's name does not
  -- exist in combat wheel (i.e. a single rotation without update)
  delay :: CombatWheel -> String -> CombatWheel
  delay w s = moveAfter w (\c -> fst c == s)


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
  alDoc al = text "[ " <> P.vcat ( map ( text . (\(a,b) -> show a ++ " => " ++ show b)) al) <> text " ]"



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



  -- More to come


  -- Some test dudes
  dummy1 = createCharacter "Dummy 1" 4
  dummy2 = createCharacter "Dummy 2" 1
  dummy3 = createCharacter "Dummy 3" 14
  dummy4 = createCharacter "Dummy 4" 20

  effect1 = ("Magical Effect Foo", [("Duration", Int 4)])

  combat1 = addCharacters (effect1:combat) [dummy3, dummy2, dummy1]

  example1 = combatDoc combat1

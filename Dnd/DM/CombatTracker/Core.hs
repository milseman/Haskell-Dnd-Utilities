module Dnd.DM.CombatTracker.Core where
  import Data.AssocList
  import Data.Wheel

  import Data.List
  import Control.Monad.State

  import qualified Text.PrettyPrint.HughesPJ as P
  import Text.PrettyPrint.HughesPJ (($+$), (<>), ($$), text, nest)

  type Agent = String

  -- I thought about doing this with existential types, but didn't want rank-2 types
  -- So I decided to just make this a synonym for Int
  type Info = Int

  type Entry = (Agent, AssocList String Info)
  type Character = Entry
  type Effect = Entry
  type Marker = Entry
  type Monster = Entry

  type CombatWheel = Wheel Entry

  -- | Combat wheel
  combat :: CombatWheel
  combat = [topOfRound]

  -- | Entry for the top of the round
  topOfRound :: Marker
  topOfRound = ("Top-Of-Round", [("Round-Number", 1)])

  -- | Find the current round number of combat. Undefined in the absence of a top
  -- of round marker
  roundNumber :: CombatWheel -> Int
  roundNumber w = unsafeFetch "Round-Number" (unsafeFetch "Top-Of-Round" w)

  -- | Create a character given name and initiative
  createCharacter :: String -> Int -> Character
  createCharacter s i = (s, [("Initiative", i)])

  -- | Add a character to a combat wheel
  addCharacter :: String -> Int -> CombatWheel -> CombatWheel
  addCharacter s i = insertAt (isBefore char) char Before
    where char = createCharacter s i

  -- | List version of addCharacter
  addCharacters :: [(String,Int)] -> CombatWheel -> CombatWheel
  addCharacters cs w = foldl (flip (uncurry addCharacter)) w cs

  -- | Create a monster, given name, initiative, and hp
  createMonster :: String -> Int -> Int -> Monster
  createMonster s i hp = (s, add entry "HP" ( hp))
    where entry = snd $ createCharacter s i

  -- | Add a monster to a combat wheel
  addMonster :: String -> Int -> Int -> CombatWheel -> CombatWheel
  addMonster s i hp = insertAt (isBefore mon) mon Before
    where mon = createMonster s i hp

  -- | Create an effect given name and duration
  createEffect :: String -> Int -> Effect
  createEffect s i = (s, [("Duration", i)])

  -- | Add an effect, such as an ongoing spell, and insert it at the given position
  -- relative to the specified entry
  addEffect :: String -> Int -> Position -> String -> CombatWheel -> CombatWheel
  addEffect s i pos entry = insertAt (isEntry entry) (createEffect s i) pos


  -- | Remove the Entry whose name is passed in
  remove :: String -> CombatWheel -> CombatWheel
  remove = flip del


  -- | Damage a monster
  damage :: String -> Int -> CombatWheel -> CombatWheel
  damage s i w = replace (createMonster s initiative newhp) (isEntry s) w
    where initiative = unsafeFetch "Initiative" monster
          newhp = unsafeFetch "HP" monster - i
          monster = unsafeFetch s w

  -- | Heal a monster
  -- Does NOT know or follow any game mechanics, such as max hp or tmp hp
  heal :: String -> Int -> CombatWheel -> CombatWheel
  heal s i = damage s (0-i)


  -- | Makes the top most entry delay until after the provided character's turn
  -- Will delay until the end of the wheel if given character's name does not
  -- exist in combat wheel (i.e. a single rotation without update)
  -- Note: Doesn't actually update the initiative value
  delay :: String -> CombatWheel -> CombatWheel
  delay s = move (isEntry s) After


  -- | Advance the round, processing durations, round numbers, hp etc
  -- Does NOT know or follow game mechanics, such as bleeding out
  advance :: CombatWheel -> CombatWheel
  advance (e:es) | or [isOver e, isDead e] = es
  advance (e:es) | isUnconscious e = es ++ [e]  -- skip its turn
  advance w = process updateEntry w


  -- | Pretty Printing

  -- | Doc for CombatWheels
  combatDoc :: CombatWheel -> P.Doc
  combatDoc w =
    text "Combat: " <> text ("(round " ++ show (roundNumber w) ++ ")")
      $$ nest 4 (P.vcat (map ((flip entryDoc) nesting) w))
        where nesting = 6 + maximum (map (length . fst) w) -- pad for unconscious

  -- | Doc for Entries
  entryDoc :: Entry -> Int -> P.Doc
  entryDoc (s, al) nesting = entryname <> text (":") $$ nest nesting (alDoc al)
    where entryname = text $ if isUnconscious (s, al) then "<" ++ s ++ ">" else s

  -- | Doc for Association Lists
  alDoc :: (Show b) => AssocList String b -> P.Doc
  alDoc ((a,b):al) = P.vcat (text "[ " <> innard a b : rest al) <> text " ]"
    where innard a b = text $ a ++ " => " ++ show b
          rest al = map (\(a,b) -> text ", " <> innard a b) al

  -- | Pretty Print a combat wheel
  pp :: CombatWheel -> IO()
  pp = putStrLn . P.render . combatDoc


  -- | Utility functions

  -- | Initiative of a character
  initOf :: Character -> Maybe Info
  initOf = lookup "Initiative" . snd

  -- | Predicate telling whether the first argument comes before the second. Attempts
  -- to look for the field Initiative in the second argument, at which points it
  -- compares. If Initiative is not present, or is non-integer, then false is
  -- returned.
  -- Undefined if the first argument has no integer Initiative.
  isBefore :: Character -> Character -> Bool
  isBefore char char2 =
    case (initOf char, initOf char2) of
      (Just ( left), Just ( right)) -> left > right
      (Just ( i), _) -> False

  -- | Predicate to match an entry with it's name
  isEntry :: String -> Entry -> Bool
  isEntry s c = fst c == s

  -- | Update the entry, i.e. decrement durations, increase turn counts, etc.
  updateEntry :: Entry -> Entry
  updateEntry (a, al) = (a, handleThese ["Duration", "Round-Number"] al)
    where handleThese (x:xs) al = handle x (handleThese xs al)
          handleThese [] al = al
          handle "Duration" al = case lookup "Duration" al of
                                   Just ( i) -> incr "Duration" (i-1)
                                   Nothing -> al
          handle "Round-Number" al = case lookup "Round-Number" al of
                                       Just ( i) -> incr "Round-Number" (i+1)
                                       Nothing -> al
          incr s i = add al s i

  -- | Predicate saying whether the current effect is over (i.e. Duration 0)
  -- If Duration is missing or positive, then false. Else true
  isOver :: Entry -> Bool
  isOver (a, al) = case lookup "Duration" al of
                     Just i -> i <= 0
                     _ -> False

  -- | Predicate saying whether the current monster is unconscious
  -- If HP is missing or > 0 then False, else True
  isUnconscious :: Entry -> Bool
  isUnconscious (a, al) = case lookup "HP" al of
                            Just i -> i < 0
                            Nothing -> False

  -- | Predicate saying whether the current monster is dead
  -- If HP is missing or > -10 then False, else True
  isDead :: Entry -> Bool
  isDead (a, al) = case lookup "HP" al of
                            Just i -> i <= -10
                            Nothing -> False


  -- Posible todos: handle monster's being unconscious, and bleeding out


  -- More to come

  -- Some test dudes
  combatinitial = addCharacters [ ("Dummy 1",4), ("Dummy 2",1), ("Dummy 3", 14)
                                , ("Dummy 4",20) ] combat
  combat1a = addMonster "Spider 1" 10 4 combatinitial
  combat1 = addMonster "Wolf 1" 16 10 combat1a
  combat2 = addEffect "Wall-Of-Flame" 4 After "Top-Of-Round" combat1
  combat3 = damage "Wolf 1" 3 $ combat2
  combat4 = advance $ advance combat3
  combat5 = delay "Spider 1" combat4
  combat6 = damage "Spider 1" 15 combat5
  combat7 = advance $ advance combat6
  combat8 = heal "Wolf 1" 2 combat7

  example1 = combatDoc combat1
  example2 = combatDoc combat2
  example3 = combatDoc combat3
  example4 = combatDoc combat4
  example5 = combatDoc combat5
  example6 = combatDoc combat6
  example7 = combatDoc combat7
  example8 = combatDoc combat8

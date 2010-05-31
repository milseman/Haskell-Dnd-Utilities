module Dnd.DM.CombatTracker.Core where
  import Data.AssocList
  import Data.Wheel hiding (check)

  import Data.List
  import Control.Monad.State

  import qualified Text.PrettyPrint.HughesPJ as P
  import Text.PrettyPrint.HughesPJ (($+$), (<>), ($$), text, nest)

  type FieldName = String
  type EntryName = String

  -- I thought about doing this with existential types, but didn't want rank-2 types
  -- So I decided to just make this a synonym for Int
  type Info = Int
  type Initiative = Int
  type HP = Int

  type Entry = (EntryName, AssocList FieldName Info)
  type Character = Entry
  type Effect = Entry
  type Marker = Entry
  type Monster = Entry

  type CombatWheel = Wheel Entry
  type CombatAction = CombatWheel -> CombatWheel

  -- | Runtime exception for when a specified entry is not found
  entryNotFound = throw $ ErrorCall "Entry Not Found"

  -- | Combat wheel
  combat :: CombatWheel
  combat = [topOfRound]

  -- | Entry for the top of the round
  topOfRound :: Marker
  topOfRound = ("Top-Of-Round", [("Round-Number", 1)])

  -- | Find the current round number of combat. Throws AssocList.notFound
  roundNumber :: CombatWheel -> Int
  roundNumber w = unsafeFetch "Round-Number" (unsafeFetch "Top-Of-Round" w)

  -- | Create a character given name and initiative
  createCharacter :: EntryName -> Int -> Character
  createCharacter s i = (s, [("Initiative", i)])

  -- | Add a character to a combat wheel, based on initiative
  -- If nothing else has an initiative, then put it last
  addCharacter :: EntryName -> Int -> CombatAction
  addCharacter s i = insertAtElseEnd (isBefore char) char Before
    where char = createCharacter s i

  -- | Add a character to a specified location. Throws entryNotFound
  addCharacterAt :: EntryName -> Int -> Position -> EntryName -> CombatAction
  addCharacterAt s i pos target w =
    check target w $ insertAt (isEntry target) (createCharacter s i) pos w

  -- | List version of addCharacter
  addCharacters :: [(EntryName,Int)] -> CombatAction
  addCharacters cs w = foldl (\w (s,i) -> addCharacter s i w) w cs

  -- | Create a monster, given name, initiative, and hp
  createMonster :: EntryName -> Int -> HP -> Monster
  createMonster s i hp = (s, add entry "HP" ( hp))
    where entry = snd $ createCharacter s i

  -- | Add a monster to a combat wheel
  addMonster :: EntryName -> Int -> HP -> CombatAction
  addMonster s i hp = insertAtElseEnd (isBefore mon) mon Before
    where mon = createMonster s i hp

  -- | Add a monster to a specified location. Throws entryNotFound
  addMonsterAt :: EntryName -> Int -> HP -> Position -> EntryName -> CombatAction
  addMonsterAt s i hp pos target w =
    check target w $ insertAt (isEntry target) (createMonster s i hp) pos w

  -- | List version of addMonster
  addMonsters :: [(EntryName,Int,HP)] -> CombatAction
  addMonsters cs w = foldl (\w (s,i,hp) -> addMonster s i hp w) w cs

  -- | Create an effect given name and duration
  createEffect :: EntryName -> Int -> Effect
  createEffect s i = (s, [("Duration", i)])

  -- | Add an effect at the entry after the topmost one. If wheel is empty, just
  -- inject it
  addEffect :: EntryName -> Int -> CombatAction
  addEffect s i (e:es) = e : createEffect s i : es
  addEffect s i [] = [createEffect s i]

  -- | Add an effect, such as an ongoing spell, and insert it at the given position
  -- relative to the specified entry. Throws entryNotFound
  addEffectAt :: EntryName -> Int -> Position -> EntryName -> CombatAction
  addEffectAt s i pos target w =
    check target w $ insertAt (isEntry target) (createEffect s i) pos w

  -- | Remove the Entry whose name is passed in
  remove :: EntryName -> CombatAction
  remove = flip del

  -- | Move the Entry named to pos relative to third argument
  -- Note: undefined when the entry name is not present
  move :: EntryName -> Position -> EntryName -> CombatAction
  move tomove pos target w = insertAt (isEntry target) entry pos (del w tomove)
    where entry = (tomove, unsafeFetch tomove w)

  -- | Update a field in the named Entry to a new value, adding it if it's missing
  updateField :: EntryName -> FieldName -> Info -> CombatAction
  updateField s fieldname i (e:es) = if isEntry s e
                                     then (s, add (snd e) fieldname i) : es
                                     else e : updateField s fieldname i es

  -- | Remove a field in the named Entry
  removeField :: EntryName -> FieldName -> CombatAction
  removeField s fieldname (e:es) = if isEntry s e
                                   then (s, del (snd e) fieldname) : es
                                   else e : removeField s fieldname es


  -- | Damage a monster
  damage :: EntryName -> Int -> CombatAction
  damage s i w = replace (createMonster s initiative newhp) (isEntry s) w
    where initiative = unsafeFetch "Initiative" monster
          newhp = unsafeFetch "HP" monster - i
          monster = unsafeFetch s w

  -- | Heal a monster
  -- Does NOT know or follow any game mechanics, such as max hp or tmp hp
  heal :: EntryName -> Int -> CombatAction
  heal s i = damage s (0-i)


  -- | Makes the top most entry delay until after the provided character's turn
  -- Will delay until the end of the wheel if given character's name does not
  -- exist in combat wheel (i.e. a single rotation without update)
  -- Note: Doesn't actually update the initiative value
  delay :: EntryName -> CombatAction
  delay s = moveTo (isEntry s) After


  -- | Advance the round, processing durations, round numbers, hp etc
  -- Does NOT know or follow game mechanics, such as bleeding out
  advance :: CombatAction
  advance (e:es) | or [isOver e, isStatus Dead e] = es
  advance (e:es) | isStatus Unconscious e = es ++ [e]  -- skip its turn
  advance w = process updateEntry w


  -- | Pretty Printing

  -- | Doc for CombatWheels
  combatDoc :: CombatWheel -> P.Doc
  combatDoc w =
    text "Combat: " <> text ("(round " ++ show (roundNumber w) ++ ")")
      $$ nest 4 (P.vcat (drawBoxes entries))
        where entries = (map ((flip entryDoc) nesting) w)
              nesting = 4 + maximum (map (length . fst) w) -- pad for unconscious

  -- | Doc for Entries
  entryDoc :: Entry -> Int -> P.Doc
  entryDoc (s, al) nesting = entryname <> text (":") $$ nest nesting (alDoc al)
    where entryname = text $ left ++ s ++ right
          (left,right) = case getStatus (s, al) of
                           Conscious   -> ("","")
                           Staggered   -> ("*","*")
                           Unconscious -> ("<", ">")
                           Dead        -> ("[", "]")
                           UNDEFINED   -> ("","")

  -- | Doc for Association Lists
  alDoc :: (Show b) => AssocList String b -> P.Doc
  alDoc ((a,b):al) = P.vcat (innard a b : rest al)
    where innard a b = text $ a ++ " => " ++ show b
          rest al = map (\(a,b) -> innard a b) al
  alDoc [] = P.empty

  -- | have a line of ---- as long as the longest line in doc
  drawlines :: P.Doc -> P.Doc
  drawlines doc = text (replicate maxLen '-' )
    where maxLen = 4 + (maximum . map length . lines . P.render) doc

  -- | Draw a box around each doc in a list
  drawBoxes :: [P.Doc] -> [P.Doc]
  drawBoxes docs = lines : intersperse lines (drawPipes docs) ++ [lines]
    where lines = drawlines $ P.vcat docs

  -- | Draw pipes around each doc in a list
  drawPipes :: [P.Doc] -> [P.Doc]
  drawPipes docs = map (surroundPipes maxLen) docs
      where maxLen = maximum . map length . lines $ concatMap (\d -> P.render d ++ "\n") docs


  surroundPipes len d = P.vcat . map surr . lines $ P.render d
    where surr s = text ("| " ++ s ++ remainingSpaces s ++ " |")
          remainingSpaces s = replicate (len - length s) ' '


  -- | Pretty Print a combat wheel
  pp :: CombatWheel -> String
  pp w = P.render $ drawlines doc $$ doc $$ drawlines doc
    where doc = combatDoc w


  -- | Utility functions

  -- | Initiative of a character
  initOf :: Character -> Maybe Info
  initOf = lookup "Initiative" . snd

  -- | Predicate telling whether the first argument comes before the second. Attempts
  -- to look for the field Initiative in the second argument, at which points it
  -- compares. If Initiative is not present in either argument, false is returned.
  isBefore :: Character -> Character -> Bool
  isBefore char char2 =
    case (initOf char, initOf char2) of
      (Just left, Just right) -> left > right
      (_,_) -> False

  -- | Predicate to match an entry with it's name
  isEntry :: EntryName -> Entry -> Bool
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

  -- | Predicate saying whether any entry has an Initiative
  anyInitiative :: CombatWheel -> Bool
  anyInitiative = or . map (hasKey "Initiative") . map snd

  data Status = Conscious | Unconscious | Dead | Staggered | UNDEFINED
              deriving (Eq, Show)

  -- | Predicate saying whether the passed monster is status or not
  isStatus :: Status -> Entry -> Bool
  isStatus status e = status == getStatus e

  getStatus :: Entry -> Status
  getStatus (a, al) = case lookup "HP" al of
                        Just i -> status i
                        Nothing -> UNDEFINED
    where status i | i > 0                = Conscious
          status i | i == 0               = Staggered
          status i | and [i > -10, i < 0] = Unconscious
          status i | i <= -10             = Dead


  -- Internal checker, check to see if an entry is present, otherwise throw
  -- entryNotFound.
  -- Takes advantage of laziness to implement control flow what wont evaluate
  -- the second argument if check fails
  check :: EntryName -> CombatWheel -> CombatWheel -> CombatWheel
  check e w safeW | any (isEntry e) w = safeW
  check _ _ _ = entryNotFound


  -- More to come

  -- Some test dudes
  combatinitial = addCharacters [ ("Dummy 1",4), ("Dummy 2",1), ("Dummy 3", 14)
                                , ("Dummy 4",20) ] combat
  combat1 = addMonsters [("Wolf 1",16,10), ("Spider 1",10,4)] combatinitial
  combat2 = addEffectAt "Wall-Of-Flame" 4 After "Top-Of-Round" combat1
  combat3 = damage "Wolf 1" 3 $ combat2
  combat4 = advance $ advance combat3
  combat5 = delay "Spider 1" combat4
  combat6 = damage "Spider 1" 15 combat5
  combat7 = advance $ advance combat6
  combat8 = heal "Wolf 1" 2 combat7
  combat9 = advance $ damage "Wolf 1" 9 combat8
  combat10 = damage "Wolf 1" 2 combat9

  example1 = combatDoc combat1
  example2 = combatDoc combat2
  example3 = combatDoc combat3
  example4 = combatDoc combat4
  example5 = combatDoc combat5
  example6 = combatDoc combat6
  example7 = combatDoc combat7
  example8 = combatDoc combat8
  example9 = combatDoc combat9
  example10 = combatDoc combat10

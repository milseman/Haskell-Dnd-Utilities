module Dnd.DM.CombatTracker.CTController where
  import Data.Wheel
  import Dnd.DM.CombatTracker.Core

  import Control.Monad.State

  type Name = String
  type Field = String
  type Pos = Position

  type Duration = Int
  type Init = Int
  type HP = Int
  type Turns = Int
  type Value = Int

  -- | Combat state
  type CombatState = StateT CombatWheel IO ()

  -- | Interpreter commands, Impl is for the implicit version of the command
  data Command = Character Name Init Pos Name  | CharacterImplicit Name Init
               | Monster Name Init HP Pos Name | MonsterImplicit Name Init HP
               | Effect Name Duration Pos Name | EffectImplicit Name Duration
               | Damage Name HP | Heal Name HP
               | Delay Name
               | Next Turns | NextImplicit
               | Move Name Pos Name
               | Remove Name
               | Update Name Field Value | RemoveField Name Field
--               | Undo Turns | UndoImplicit | Redo Turns | RedoImplicit


  -- todo: find a good way/place to verify before attempting an action

  -- | Dispatch from a Command to modify the combat state
  controller :: Command -> CombatState
  controller (CharacterImplicit name init) = modify $ addCharacter name init
  controller (Character name init pos name2) =
    modify $ insertAt (isEntry name2) (createCharacter name init) pos
  controller (MonsterImplicit name init hp) = modify $ addMonster name init hp
  controller (Monster name init hp pos name2) =
    modify $ insertAt (isEntry name2) (createMonster name init hp) pos
  controller (EffectImplicit name duration) =
    modify $ \w -> addEffect name duration After (fst (head w)) w
  controller (Effect name duration pos name2) =
    modify $ addEffect name duration pos name2
  controller (Damage name hp) = modify $ damage name hp
  controller (Heal name hp) = modify $ heal name hp
  controller (Delay name) = modify $ delay name
  controller (NextImplicit) = modify $ advance
  controller (Next turns) = modify $ applyN turns advance
  controller (Move name pos name2) = modify $ move name pos name2
  controller (Remove name) = modify $ remove name
  controller (Update name field value) = modify $ updateField name field value
  controller (RemoveField name field) = modify $ removeField name field

  -- todo add other commands

  -- Applys the nth composition of f to x
  -- undefined for negative n
  applyN :: Int -> (a -> a) -> a -> a
  applyN n f x | n > 0 = applyN (n-1) f (f x)
  applyN 0 f x = x

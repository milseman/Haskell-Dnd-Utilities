module Dnd.DM.CombatTracker.CTController where
  import qualified Data.Wheel as W
  import Dnd.DM.CombatTracker.Core

  import Control.Monad.State

  type Name = String
  type Field = String
  type Pos = W.Position

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
--               | Move Name Pos Name
--               | Remove Name
--               | Swap Name Name
--               | Update Name Field Value
--               | Undo Turns | UndoImplicit | Redo Turns | RedoImplicit


  -- | Dispatch from a Command to modify the combat state
  controller :: Command -> CombatState
  controller (CharacterImplicit name init) = modify $ addCharacter name init
  controller (Character name init pos name2) =
    modify $ W.insertAt (isEntry name2) (createCharacter name init) pos
  controller (MonsterImplicit name init hp) = modify $ addMonster name init hp
  controller (Monster name init hp pos name2) =
    modify $ W.insertAt (isEntry name2) (createMonster name init hp) pos
  controller (EffectImplicit name duration) =
    modify $ \w -> addEffect name duration W.After (fst (head w)) w
  controller (Effect name duration pos name2) =
    modify $ addEffect name duration pos name2
  controller (Damage name hp) = modify $ damage name hp
  controller (Delay name) = modify $ delay name
  controller (NextImplicit) = modify $ advance
  controller (Next turns) = modify $ applyN turns advance

  -- todo add other commands


  -- Applys the nth composition of f to x
  -- undefined for negative n
  applyN :: Int -> (a -> a) -> a -> a
  applyN n f x | n > 0 = applyN (n-1) f (f x)
  applyN 0 f x = x

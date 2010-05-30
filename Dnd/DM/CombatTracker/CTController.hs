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
  type Combat = StateT CombatWheel IO ()

  -- | Interpreter commands, Impl is for the implicit version of the command
  data Command = Character Name Init Pos Name  | CharacterImplicit Name Init
               | Monster Name Init HP Pos Name | MonsterImplicit Name Init HP
               | Effect Name Duration Pos Name | EffectImplicit Name Duration
               | Damage Name HP | Heal Name HP 
               | Delay Pos Name
               | Next Turns | NextImplicit
               | Move Name Pos Name | Remove Name | Swap Name Name
               | Update Name Field Value
               | Undo Turns | UndoImplicit | Redo Turns | RedoImplicit

    
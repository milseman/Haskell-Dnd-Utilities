 module Dnd.DM.CombatTracker.CTController where
  import Data.Wheel
  import Dnd.DM.CombatTracker.Core

  import Control.Monad.State
  import Control.Monad.Error
  import Control.Exception.Base as E

  type Name = String
  type Field = String
  type Pos = Position

  type Duration = Int
  type Init = Int
  type HP = Int
  type Turns = Int
  type Value = Int

  -- | A placed to stick error messages
  type CombatError = ErrorT String IO

  -- | Combat state
  type CombatState = StateT CombatWheel CombatError ()

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
    do w <- get
       check name2 w $ modify $ addCharacterAt name init pos name2
  controller (MonsterImplicit name init hp) = modify $ addMonster name init hp
  controller (Monster name init hp pos name2) =
    do w <- get
       check name2 w $ modify $ addMonsterAt name init hp pos name2
  controller (EffectImplicit name duration) = modify $ addEffect name duration
  controller (Effect name duration pos name2) =
    do w <- get
       check name2 w $ modify $ addEffectAt name duration pos name2
  controller (Damage name hp) =
    do w <- get
       check name w $ modify $ damage name hp
  controller (Heal name hp) =
    do w <- get
       check name w $ modify $ heal name hp
  controller (Delay name) =
    do w <- get
       check name w $ modify $ delay name
  controller (NextImplicit) = modify $ advance
  controller (Next turns) = modify $ applyN turns advance
  controller (Move name pos name2) =
    do w <- get
       check name w $ check name w $ modify $ move name pos name2
  controller (Remove name) = modify $ remove name
  controller (Update name field value) =
    do w <- get
       check name w $ modify $ updateField name field value
  controller (RemoveField name field) =
    do w <- get
       check name w $ modify $ removeField name field

  -- todo add other commands

  -- Checks for existance before undertaking an operation. Integrates with CombatState
  check :: EntryName -> CombatWheel -> CombatState -> CombatState
  check e w c | any (isEntry e) w = c
  check e _ _ = throwError "Entry Not Found"


  -- Applys the nth composition of f to x
  -- undefined for negative n
  applyN :: Int -> (a -> a) -> a -> a
  applyN n f x | n > 0 = applyN (n-1) f (f x)
  applyN 0 f x = x

  catchNotFound :: CombatWheel -> (ErrorCall -> IO CombatWheel) -> IO CombatWheel
  catchNotFound w h = E.catch (evaluate w) h

  handleNotFound w (ErrorCall s) = do putStrLn s
                                      return w

  tester = do w <- execStateT (controller (Character "foo" 4 Before "bar")) combat
              liftIO $ putStrLn $ pp $ w
           `catchError` (\e -> liftIO ( putStrLn ("caught" ++ e)))
  testShowable = do e <- runErrorT tester
                    putStrLn $ case e of Left w -> w
                                         Right _ -> ""


  -- myHandler :: ErrorCall -> IO CombatWheel
  -- myHandler =
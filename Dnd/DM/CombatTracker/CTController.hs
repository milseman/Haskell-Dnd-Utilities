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
  type CombatState = StateT [CombatWheel] CombatError ()

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
               | Undo Turns | UndoImplicit -- | Redo Turns | RedoImplicit
               | Show


  -- todo: find a good way/place to verify before attempting an action

  -- | Dispatch from a Command to modify the combat state
  controller :: Command -> CombatState
  controller (CharacterImplicit name init) =
    record $ addCharacter name init
  controller (Character name init pos name2) =
    do ws <- get
       safeRecord [name2] ws $ addCharacterAt name init pos name2
  controller (MonsterImplicit name init hp) = record $ addMonster name init hp
  controller (Monster name init hp pos name2) =
    do ws <- get
       safeRecord [name2] ws $ addMonsterAt name init hp pos name2
  controller (EffectImplicit name duration) = record $ addEffect name duration
  controller (Effect name duration pos name2) =
    do ws <- get
       safeRecord [name2] ws $ addEffectAt name duration pos name2
  controller (Damage name hp) =
    do ws <- get
       safeRecord [name] ws $ damage name hp
  controller (Heal name hp) =
    do ws <- get
       safeRecord [name] ws $ heal name hp
  controller (Delay name) =
    do ws <- get
       safeRecord [name] ws $ delay name
  controller (NextImplicit) = record $ advance
  controller (Next turns) = record $ applyN turns advance
  controller (Move name pos name2) =
    do ws <- get
       safeRecord [name, name2] ws $ move name pos name2
  controller (Remove name) = record $ remove name
  controller (Update name field value) =
    do ws <- get
       safeRecord [name] ws $ updateField name field value
  controller (RemoveField name field) =
    do ws <- get
       safeRecord [name] ws $ removeField name field
  controller UndoImplicit = do { ws <- get ; safePop ws 1 }
  controller (Undo i) = do { ws <- get ; safePop ws i }
  -- Throws the current wheel, pretty printed (I might want to change this)
  controller Show = do { ws <- get ; throwError (pp (head ws)) }

  -- todo add other commands

  -- Checks for existance before undertaking an operation.
  checkPresence :: EntryName -> CombatWheel -> CombatState -> CombatState
  checkPresence e w c | any (isEntry e) w = c
  checkPresence e _ _ = throwError $ "Entry `" ++ e ++ "' Not Found"



  -- Checks that history is adequately long before popping off i items
  safePop :: [CombatWheel] -> Int -> CombatState
  safePop ws i | length ws < i =
    throwError $ "History only contains " ++ show (length ws) ++ " entries"
  safePop ws i = modify $ \_ -> drop i ws


  -- Applys the nth composition of f to x
  -- undefined for negative n
  applyN :: Int -> (a -> a) -> a -> a
  applyN n f x | n > 0 = applyN (n-1) f (f x)
  applyN 0 f x = x

  -- catchNotFound :: CombatWheel -> (ErrorCall -> IO CombatWheel) -> IO CombatWheel
  -- catchNotFound w h = E.catch (evaluate w) h

  -- handleNotFound w (ErrorCall s) = do putStrLn s
  --                                     return w

  -- tester = do w <- execStateT (controller (Character "foo" 4 Before "bar")) combat
  --             liftIO $ putStrLn $ pp $ w
  --          `catchError` (\e -> liftIO ( putStrLn ("caught" ++ e)))
  -- testShowable = do e <- runErrorT tester
  --                   putStrLn $ case e of Left w -> w
  --                                        Right _ -> ""


  -- Apply a function to the top element of a stack, and return its result pushed
  -- onto the passed stack.
  -- Undefined for an empty stack
  applyPush :: (a -> a) -> [a] -> [a]
  applyPush f (x:xs) = f x : x : xs

  -- Perform an operation to the top of your state record, and record your result
  -- Record as in the verb. And is in what a VCR does. Not to be confused with the
  -- Haskell data structure.
  record = modify . applyPush

  -- Record, but first checks that entries are present in the current wheel
  -- Safe as in safe with respect to entrynames being present
  -- Throws Entry Not Found
  safeRecord :: [EntryName] -> [CombatWheel] -> (CombatAction) -> CombatState
  safeRecord (e:es) (w:ws) c = checkPresence e w $ safeRecord es (w:ws) c
  safeRecord [] ws c = record c
module Dnd.DM.CombatTracker.CTInterpreter where
  import Data.Wheel as W
  import Dnd.DM.CombatTracker.Core

  import Control.Monad.State
  

  -- | Combat state
  type Combat = StateT CombatWheel IO ()

  data Command = Character | Event | Monster 


  -- | Interpreter
  --   Types not given, as they are all just IO()

  -- | Execute the program
  main = do initialize
            repl

  -- | Read-eval-print loop
  repl = do echo
            repl

  -- | Echo what is read
  echo = do ln <- getLine 
            putStrLn ln



  -- | Take care of any initializations (right now just welcome the individual
  initialize = do welcome

  -- | Welcome the individual
  welcome = putStrLn "Interpreter for the CombatTracker system"                  
            


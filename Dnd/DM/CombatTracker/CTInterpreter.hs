module Dnd.DM.CombatTracker.CTInterpreter where
  import qualified Data.Wheel as W
  import Dnd.DM.CombatTracker.Core

  import Control.Monad.State
  import qualified Data.List as L
  import System.Console.Readline -- require haskell readline package

  {- Note for those trying to install haskell-readline on Arch:
     GHC is unable to properly understand linker scripts, so replace
     /usr/lib/ncurses.so with a symlink to the proper ncurses.so
     e.g: sudo ln -s libncurses.so.5 libncurses.so
   -}

  type Name = String
  type Field = String
  type CommandName = String
  type Position = W.Position

  type Duration = Int
  type Initiative = Int
  type HP = Int
  type Turns = Int
  type Value = Int


  -- | Combat state
  type Combat = StateT CombatWheel IO ()

  -- | Interpreter commands
  data Command = Character Name Initiative Position Name
               | Monster Name Initiative HP Position Name
               | Effect Name Duration Position Name
               | Damage Name HP | Heal Name HP
               | Delay Position Name
               | Next Turns
               | Move Name Position Name | Remove Name | Swap Name Name
               | Update Name Field Value
               | Undo Turns | Redo Turns
               | Help CommandName

  -- | The commands available for the user to type in
  commands = [ "character", "monster", "effect", "damage", "delay", "next"
             , "move", "remove", "swap", "update", "undo", "redo"]

  -- | Help documentation
  help "meta"      = "<pos>: before | after"
                     ++ "\n<(entry-)name|field>: String (no spaces)"
                     ++ "\n<initiative|hp|duration|turns|field>: Int"
  help "help"      = "Usage: help <command>"
                     ++ "\nSpecial: `help commands' lists all avaliable commands"
  help "commands"  = "Avaliable commands: \n  " ++ concatMap (\c -> c++", ") commands
                     ++ "\nUse `help <command>' to see help on a particular command"
  help "character" = "Usage: character <name> <initiative> [<pos> <entry-name>]"
                     ++ "\nDefault: insert according to provided initiative"
  help "monster"   = "Usage: monster <name> <initiative> <hp> [<pos> <entry-name>]"
                     ++ "\nDefault: insert according to provided initiative"
  help "effect"    = "Usage: effect <name> <duration> [<pos> <entry-name>]"
                     ++ "\nDefault: insert at top"
  help "damage"    = "Usage: damage <name> <hp>"
  help "delay"     = "Usage: delay <pos> <entry-name>"
  help "next"      = "Usage: next [<turns>]"
                     ++ "\nDefault: 1"
  help "move"      = "Usage: move <name> <pos> <name>"
  help "remove"    = "Usage: remove"
  help "swap"      = "Usage: swap <name> <name>"
  help "update"    = "Usage: update <name> <field> <value>"
  help "undo"      = "Usage: undo <turns> !! Not yet available"
  help "redo"      = "Usage: redo <turns> !! Not yet available"


  -- | Interpreter

  --  Types not given, as they are all just IO()

  -- | Execute the program
  main = do initialStuff
            repl

  -- | Read-eval-print loop
  repl = do ln <- readline ">> "
            case ln of Just s -> do { addHistory s; interpret s; repl }
                       Nothing -> return () -- EOF

  -- | Interpret a line
  interpret "" = putStrLn $ "Please enter a command\n\t" ++ help "commands"
  interpret s = if command `elem` commands then commandHandler command args
                else putStrLn $ "Error: command `" ++ command ++ "' not found" ++
                     "\n" ++ help "commands"
    where (command:args) = words s


  commandHandler "character" [n,init,pos,n2] = echo ""
  commandHandler "character" [n,init] = echo ""
  commandHandler "monster" [n,init,hp,pos,n2] = echo ""
  commandHandler "monster" [n,init,hp] = echo ""
  commandHandler "effect" [n,dur,pos,n2] = echo ""
  commandHandler "effect" [n,dur] = echo ""
  commandHandler "damage" [n,dam] = echo ""
  commandHandler "delay" [n] = echo ""
  commandHandler "next" [i] = echo ""
  commandHandler "next" [] = echo ""
  commandHandler "move" [n,pos,n2]= echo ""
  commandHandler "remove" [n] = echo ""
  commandHandler "swap" [n,n2] = echo ""
  commandHandler "update" [n,field,val] = echo ""
  commandHandler "undo" [i] = echo ""
  commandHandler "undo" []  = echo""                          
  commandHandler "redo" [i] = echo ""
  commandHandler "redo" []  = echo""                          
  commandHandler "help" [c] = echo ""
  commandHandler "help" [] = echo ""
  commandHandler c _ = putStrLn $ "Error: ill-formed command\n" ++ help c

  -- | Echo what is read
  echo = putStrLn


  -- | Take care of any initializations (right now just welcome the individual
  initialStuff = do welcome

  -- | Welcome the individual
  welcome = putStrLn "Interpreter for the CombatTracker system"



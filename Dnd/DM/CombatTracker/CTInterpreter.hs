module Dnd.DM.CombatTracker.CTInterpreter where
  import qualified Data.Wheel as W
  import Dnd.DM.CombatTracker.Core
  import Dnd.DM.CombatTracker.CTController

  import qualified Data.List as L
  import System.Console.Readline -- require haskell readline package

  {- Note for those trying to install haskell-readline on Arch:
     GHC is unable to properly understand linker scripts, so replace
     /usr/lib/ncurses.so with a symlink to the proper ncurses.so
     e.g: sudo ln -s libncurses.so.5 libncurses.so
   -}


  -- | The commands available for the user to type in
  commands = [ "character", "monster", "effect", "damage", "delay", "next"
             , "move", "remove", "swap", "update", "undo", "redo", "help" ]

  -- | Help documentation
  help "meta"      = "  <pos>: before | after"
                     ++ "\n  <(entry-)name|field>: String (no spaces)"
                     ++ "\n  <initiative|hp|duration|turns|field>: Int"
  help "help"      = "  Usage: help <command>"
                     ++ "\n  Special: `help commands' lists all avaliable commands"
  help "commands"  = "Avaliable commands: \n  " ++ concatMap (\c -> c++", ") commands
                     ++ "\nUse `help <command>' to see help on a particular command"
  help "character" = "  Usage: character <name> <initiative> [<pos> <entry-name>]"
                     ++ "\n  Default: insert according to provided initiative"
  help "monster"   = "  Usage: monster <name> <initiative> <hp> [<pos> <entry-name>]"
                     ++ "\n  Default: insert according to provided initiative"
  help "effect"    = "  Usage: effect <name> <duration> [<pos> <entry-name>]"
                     ++ "\n  Default: insert at top"
  help "damage"    = "  Usage: damage <name> <hp>"
  help "delay"     = "  Usage: delay <pos> <entry-name>"
  help "next"      = "  Usage: next [<turns>]"
                     ++ "\n  Default: 1"
  help "move"      = "  Usage: move <name> <pos> <name>"
  help "remove"    = "  Usage: remove"
  help "swap"      = "  Usage: swap <name> <name>"
  help "update"    = "  Usage: update <name> <field> <value>"
  help "undo"      = "  Usage: undo <turns> !! Not yet available"
  help "redo"      = "  Usage: redo <turns> !! Not yet available"
  help ""          = help "help"
  help c           = "Error: command `" ++ c ++ "' not found\n" ++ help "commands"

  -- | Interpreter

  -- | Execute the program
  main = do initialStuff
            repl

  -- | Read-eval-print loop
  repl = do ln <- readline ">> "
            case ln of Just s -> do { addHistory s; interpret s; repl }
                       Nothing -> return () -- EOF

  -- | Interpret a line
  interpret "" = putStrLn $ "Please enter a command\n" ++ help "commands"
  interpret s = if command `elem` commands then commandHandler command args
                else putStrLn $ help command
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
  commandHandler "help" [c] = echo $ help c
  commandHandler "help" [] = echo $ help "help"
  commandHandler c _ = putStrLn $ "Error: ill-formed command\n" ++ help c

  -- | Echo what is read
  echo = putStrLn

  -- | Take care of any initializations (right now just welcome the individual)
  initialStuff = do welcome

  -- | Welcome the individual
  welcome = putStrLn "Interpreter for the CombatTracker system"

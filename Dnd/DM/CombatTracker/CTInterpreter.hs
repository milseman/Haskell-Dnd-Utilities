module Dnd.DM.CombatTracker.CTInterpreter where
  import Data.Wheel
  import Dnd.DM.CombatTracker.Core
  import Dnd.DM.CombatTracker.CTController

  import Data.Char
  import qualified Data.List as L
  import Control.Monad.State
  import System.Console.Readline -- require haskell readline package

  {- Note for those trying to install haskell-readline on Arch:
     GHC is unable to properly understand linker scripts, so replace
     /usr/lib/ncurses.so with a symlink to the proper ncurses.so
     e.g: sudo ln -s libncurses.so.5 libncurses.so
   -}

  -- | The commands available for the user to type in
  commands = [ "character", "monster", "effect", "damage", "delay", "next"
             , "move", "remove", "update", "help" ] -- ++ [ "undo", "redo" ]

  -- todo: implement a general help functionality module, using pretty pringing,
  -- key value pairs, CFGs, and other cool stuff.

  -- todo: implement error handling

  -- | Help documentation
  help "meta"      = "  <pos>                                = before | after\n"
                  ++ "  <(entry-)name|field>                 = String (no spaces)\n"
                  ++ "  <initiative|hp|duration|turns|field> = Int"
  help "help"      = "  Usage: help <command>\n"
                  ++ "  Special: `help commands' lists all avaliable commands\n"
                  ++ "  Special: `help meta' shows the meta help"
  help "commands"  = "Avaliable commands: \n  " ++ concatMap (\c -> c++", ") commands
                  ++ "\n Use `help <command>' to see help on a particular command"
  help "character" = "  Description: Add a character to combat\n"
                  ++ "  Usage: character <name> <initiative> [<pos> <entry-name>]\n"
                  ++ "  Default: insert according to provided initiative"
  help "monster"   = "  Description: Add a monster to combat\n"
                  ++ "  Usage: monster <name> <initiative> <hp> [<pos> <entry-name>]\n"
                  ++ "  Default: insert according to provided initiative"
  help "effect"    = "  Description: Add an effect with a duration to combat\n"
                  ++ "  Usage: effect <name> <duration> [<pos> <entry-name>]\n"
                  ++ "  Default: insert after what's on top"
  help "damage"    = "  Description: Deal hp damage to a monster\n"
                  ++ "  Usage: damage <name> <hp>"
  help "heal"      = "  Description: Heal hp damage to a monster\n"
                  ++ "  Usage: heal <name> <hp>"
  help "delay"     = "  Description: delay until after another combat entry\n"
                  ++ "  Usage: delay <entry-name>"
  help "next"      = "  Description: advance combat\n"
                  ++ "  Usage: next [<turns>]\n"
                  ++ "  Default: 1"
  help "move"      = "  Description: Move one entry to before or after another\n"
                  ++ "  Usage: move <name> <pos> <name>"
  help "remove"    = "  Description: Remove an entry from combat\n"
                  ++ "  Usage: remove"
  help "update"    = "  Description: Update a field for an entry, adds it if missing\n"
                  ++ "  Usage: update <name> <field> <value>"
  help "removeField" = "  Description: Removes a field from an entry, if it exists\n"
                    ++ "  Usage: removeField <name> <field>"

  -- help "undo"      = "  Usage: undo <turns> !! Not yet available"
  -- help "redo"      = "  Usage: redo <turns> !! Not yet available"
  help ""          = help "help"
  help c           = "Error: command `" ++ c ++ "' not found\n" ++ help "commands"

  -- | Interpreter

  -- | Execute the program
  main :: IO ()
  main = do initialStuff
            runStateT repl combat
            return ()

  -- | Read-eval-print loop
  repl :: CombatState
  repl = do w <- get
            echo ""
            echo "--------------------"
            echo $ pp w
            echo "--------------------"
            echo ""
            ln <- liftIO $ readline ">> "
            echo ""
            case ln of Just s -> do { (io . addHistory) s; interpret s; repl }
                       Nothing -> return () -- EOF

  -- | Interpret a line
  interpret :: String -> CombatState
  interpret "" = echo $ "Please enter a command\n" ++ help "commands"
  interpret s = if command `elem` commands then commandHandler command args
                else echo $ help command
    where (command:args) = words s

  commandHandler :: String -> [String] -> CombatState
  commandHandler "character" [n,init,pos,n2] =
    controller $ Character n (read init) (readPos pos) n2
  commandHandler "character" [n,init] = controller $ CharacterImplicit n (read init)
  commandHandler "monster" [n,init,hp,pos,n2] =
    controller $ Monster n (read init) (read hp) (readPos pos) n2
  commandHandler "monster" [n,init,hp] =
    controller $ MonsterImplicit n (read init) (read hp)
  commandHandler "effect" [n,dur,pos,n2] =
    controller $ Effect n (read dur) (readPos pos) n2
  commandHandler "effect" [n,dur] = controller $ EffectImplicit n (read dur)
  commandHandler "damage" [n,dam] = controller $ Damage n (read dam)
  commandHandler "heal" [n,hp] = controller $ Heal n (read hp)
  commandHandler "delay" [n] = controller $ Delay n
  commandHandler "next" [i] | (head i == '-') = echo "Error: negative number"
  commandHandler "next" [i] = controller . Next $ read i
  commandHandler "next" [] = controller  $ NextImplicit
  commandHandler "move" [n,pos,n2]= controller $ Move n (readPos pos) n2
  commandHandler "remove" [n] = controller $ Remove n
  commandHandler "update" [n,field,val] = controller $ Update n field (read val)
  commandHandler "removeField" [n,field] = controller $ RemoveField n field
  -- commandHandler "undo" [i] = echo ""
  -- commandHandler "undo" []  = echo""
  -- commandHandler "redo" [i] = echo ""
  -- commandHandler "redo" []  = echo""
  commandHandler "help" [c] = echo $ help c
  commandHandler "help" [] = echo $ help "help"
  commandHandler c _ = echo $ "Error: ill-formed command\n" ++ help c

  -- | Echo what is read
  echo :: String -> CombatState
  echo = io . putStrLn

  -- | Take care of any initializations (right now just welcome the individual)
  initialStuff :: IO ()
  initialStuff = do welcome

  -- | Welcome the individual
  welcome :: IO ()
  welcome = putStrLn "Interpreter for the CombatTracker system"

  -- | Lift into a CombatState
  io :: IO () -> CombatState
  io = liftIO

  -- | Read in a position
  readPos :: String -> Position
  readPos s = case canonicalize s of
                "Before" -> Before
                "After" -> After

  -- | Canonicalize a string
  canonicalize (c:cs) = toUpper c : map toLower cs
  canonicalize "" = ""
module Dnd.DM.CombatTracker.CTInterpreter where
  import Data.Wheel
  import Dnd.DM.CombatTracker.Core as Core
  import Dnd.DM.CombatTracker.CTController

  import Data.Char
  import qualified Data.List as L
  import Control.Monad.State
  import Control.Monad.Error.Class
  import Control.Exception as E
  import Control.Monad.Error
  import System.Console.Readline -- require haskell readline package

  {- Note for those trying to install haskell-readline on Arch:
     GHC is unable to properly understand linker scripts, so replace
     /usr/lib/ncurses.so with a symlink to the proper ncurses.so
     e.g: sudo ln -s libncurses.so.5 libncurses.so
   -}

  -- | The commands available for the user to type in
  commands = [ "character", "monster", "effect", "damage", "heal", "delay", "next"
             , "move", "remove", "update", "show", "undo", "redo", "help" ]

  -- todo: implement a general help functionality module, using pretty pringing,
  -- key value pairs, CFGs, and other cool stuff.

  -- todo: implement error handling

  -- | Help documentation
  help "all"       = concatMap (\c -> if c /= "all" then c ++ "\n" ++ help c ++ "\n\n"
                                      else ""
                               ) commands
  help "meta"      = "  <pos>                                = before | after\n"
                  ++ "  <(entry-)name|field>                 = String (no spaces)\n"
                  ++ "  <initiative|hp|duration|turns|field> = Int"
  help "help"      = "  Usage: help <command>\n"
                  ++ "  Special: `help commands' lists all avaliable commands\n"
                  ++ "  Special: `help meta' shows the meta help\n"
                  ++ "  Special: `help all' shows all the command's help"
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
  help "show"        = "  Description: Show the current combat state\n"
                    ++ "  Usage: show"
  help "undo"        = "  Description: undo previous commands, restore the wheel\n"
                    ++ "  Usage: undo [turns]"
  help "redo"        = "  Description: redo undid commands\n"
                    ++ "  Usage: redo [turns]\n"
                    ++ "  Note: Can only be used after undos. Any modification to"
                     ++ " the wheel outside of undo will clear the redo stack"

  -- help "undo"      = "  Usage: undo <turns> !! Not yet available"
  -- help "redo"      = "  Usage: redo <turns> !! Not yet available"
  help ""          = help "help"
  help c           = "Error: command `" ++ c ++ "' not found\n" ++ help "commands"

  -- | Interpreter

  -- | Execute the program

  main = do initialStuff
            e <- runErrorT $ runStateT repl ([Core.combat],[])
            putStrLn "Exiting..."

  -- | Read-eval-print loop
  repl :: CombatState
  repl = do w <- get
            ln <- liftIO $ readline ">> "
            echo ""
            case ln of Just s -> do { (io . addHistory) s; interpret s; repl }
                       Nothing -> return () -- EOF

  -- | Interpret a line
  interpret :: String -> CombatState
  interpret "" = echo $ "Please enter a command\n" ++ help "commands"
  interpret s =
    if command `elem` commands
    then do (w,r) <- get
            do { commandHandler command args
               ; (res,_) <- get
               ; echo ""
               ; echo $ pp (head res)
               ; echo ""
               } `catchError` (\e -> do {io (putStrLn e); modify (\_ -> (w,r))})
    else echo $ help command
    where (command:args) = words s

  commandHandler :: String -> [String] -> CombatState
  commandHandler "character" [n,init,pos,n2] | areNumbers [init] =
    controller $ Character n (read init) (readPos pos) n2
  commandHandler "character" [n,init] | areNumbers [init] =
    controller $ CharacterImplicit n (read init)
  commandHandler "monster" [n,init,hp,pos,n2] | areNumbers [init, hp]=
    controller $ Monster n (read init) (read hp) (readPos pos) n2
  commandHandler "monster" [n,init,hp] | areNumbers [init, hp]=
    controller $ MonsterImplicit n (read init) (read hp)
  commandHandler "effect" [n,dur,pos,n2] | areNumbers [dur] =
    controller $ Effect n (read dur) (readPos pos) n2
  commandHandler "effect" [n,dur] | areNumbers [dur] =
    controller $ EffectImplicit n (read dur)
  commandHandler "damage" [n,dam] | areNumbers [dam] =
    controller $ Damage n (read dam)
  commandHandler "heal" [n,hp] | areNumbers [hp] =
    controller $ Heal n (read hp)
  commandHandler "delay" [n] = controller $ Delay n
  commandHandler "next" [i] | areNumbers [i] = controller . Next $ read i
  commandHandler "next" [i] | (head i == '-') = throwError "Error: negative number"
  commandHandler "next" [] = controller  $ NextImplicit
  commandHandler "move" [n,pos,n2]= controller $ Move n (readPos pos) n2
  commandHandler "remove" [n] = controller $ Remove n
  commandHandler "update" [n,field,val] | areNumbers [val] =
    controller $ Update n field (read val)
  commandHandler "removeField" [n,field] = controller $ RemoveField n field
  -- commandHandler "undo" [i] = echo ""
  -- commandHandler "undo" []  = echo""
  -- commandHandler "redo" [i] = echo ""
  -- commandHandler "redo" []  = echo""
  commandHandler "show" [] = do modify id
  commandHandler "undo" [] = controller UndoImplicit
  commandHandler "undo" [i] | areNumbers [i] = controller $ Undo (read i)
  commandHandler "redo" [] = controller RedoImplicit
  commandHandler "redo" [i] | areNumbers [i] = controller $ Redo (read i)
  commandHandler "help" [c] = throwError $ help c
  commandHandler "help" [] = throwError $ help "help"
  commandHandler c _ = throwError $ "Error: ill-formed command\n" ++ help c

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

  -- | Predicate testing whether supplied strings can be safely read as numbers
  -- Note that read erros on empty string, hence special cases for []
  areNumbers :: [String] -> Bool
  areNumbers [] = False
  areNumbers ss = and $ map allDigits ss
    where allDigits [] = False
          allDigits s = and $ map isDigit s
  -- | Handle exceptions

  -- handler :: CombatState -> CombatState
  -- handler c = catchError c handleErrorCall

  -- handleErrorCall :: SomeException -> CombatState
  -- handleErrorCall e = do io $ putStrLn e

  -- | Canonicalize a string
  canonicalize (c:cs) = toUpper c : map toLower cs
  canonicalize "" = ""
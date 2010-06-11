module Dnd.Dice.DiceInterpreter where
  import Dnd.Dice.DiceController
  import Dnd.Dice.DiceParser

  import Control.Monad.State.Lazy    

  import System.Console.Readline  -- require haskell readline package

  {- Note for those trying to install haskell-readline on Arch:
     GHC is unable to properly understand linker scripts, so replace
     /usr/lib/ncurses.so with a symlink to the proper ncurses.so
     e.g: sudo ln -s libncurses.so.5 libncurses.so
   -}

  -- | The commands available for the user to type in
  commands = ["perrin","p","crit","c","d","help"]

  -- | Help documentation
  help "all"      = concatMap (\c -> if c /= "all" then c ++ "\n" ++ help c ++ "\n\n" else "") commands
  help "meta"     = " <n|s|int> = Int"
  help "help"     = "  Usage: help <command>\n"
                 ++ "  Special: `help commands' lists all avaliable commands\n"
                 ++ "  Special: `help meta' shows the meta help\n"
                 ++ "  Special: `help all' shows all the command's help\n"
                 ++ "  Special: `help examples' shows some examples\n"
                 ++ "  Special: `help precedence shows operator precedence"
  help "commands"  = "Avaliable commands: \n  " ++ concatMap (\c -> c++", ") commands
                  ++ "\n Use `help <command>' to see help on a particular command"
  help "examples"  = "  1d6             -- roll a 6 sided die\n"
                  ++ "  1d20 + p        -- roll a 20 sided die, and add a perrin roll (see help perrin)\n"
                  ++ "  1 crit 20 + 2d6 -- roll a d20, if the result is 20 then add a perrin roll, finally add 2d6\n"
                  ++ "  2*(1+3)d6       -- roll 4d6, and multiply result by 2"
  help "precedence" = "  Order of precedence, from loosest binding to tightest binding:\n"
                   ++ "  [(+, -), *, (d, crit, c), (<int>, perrin, p, '(' <expr> ')' )] "

  help "d"      = "  Description: Roll an s-sided die n times, and sum up the results\n"
               ++ "  Usage: <n> d <s>"
  help "perrin" = "  Description: Roll a perrin crit die (1d10-1).\n"
               ++ "  If this results in a 9, repeat. Sum the results\n"
  help "p"      = "  p is an alias for perrin\n" ++ help "perrin"
               ++ "  Usage: (perrin | p)"
  help "c"      = "  c is an alias for crit\n" ++ help "crit"
  help "crit"   = "  Description: As `d', but if the result is s, add a `perrin'\n"
               ++ "  Usage: <n> (crit | c) <s>"

  help ""       = help "help"
  help c        = "Error: command `" ++ c ++ "' not found\n" ++ help "commands"


  -- | Execute the program
  main = do initialStuff
            runStateT repl []
            putStrLn "Exiting..."

  -- | Read-eval-print loop
--  repl :: DiceResult
  repl = do ln <- liftIO $ readline ">> "
            echo ""
            case ln of Just s -> (liftIO . addHistory) s >> interpret s >> repl
                       Nothing -> return () -- EOF

--  interpret :: String -> DiceResult
  interpret "" = echo $ "Please enter a formula\n" ++ help "commands"
  interpret s = if command == "help" then echo (help (concat args))
                else case parser s of 
                       Error err -> echo err >> 
                       Result res -> io . ppDiceResult . eval $ res
    where (command:args) = words s

  echo = io . putStrLn

  io :: IO () -> DiceResult
  io m = liftIO $ m >> return 0

  initialStuff = putStrLn "Interpreter for the Dice system"
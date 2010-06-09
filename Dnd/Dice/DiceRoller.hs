module Dnd.Dice.DiceRoller where
  import System.Random
  import Control.Monad
  import Control.Monad.State.Lazy

  import Data.List

  import Text.PrettyPrint.HughesPJ (($+$), (<>), ($$), text, nest)

  type CritRange = [Int]

  type RollEntry = (String, Int)
  type RollHistory = [RollEntry]

  -- A result coupled with the history of a roll
  type Roll = StateT RollHistory IO Int

  -- Create a random number
  rand :: (Int, Int) -> IO Int
  rand = randomRIO

  --  Roll a s-sided die
  rollDie :: Int -> Roll
  rollDie s = StateT $ \h -> do r <- rand (1,s)
                                return (r, addR s r h)

  -- Add a number to a roll history
  -- The first argument denotes the sidedness of the die rolled, if 0 then we assume it's a Perrin roll
  addR :: Int -> Int -> RollHistory -> RollHistory
  addR 0 r h = ("Perrin", r) : h
  addR s r h = (show s, r) : h


  -- The following used the Perrin critical system

 {- Perrin critical system:
    When you roll within your crit range, do the following:
      1) roll a 1d10-1 and add this result to your attack roll
      2) If the roll was a 9, repeat
    If any attack roll doubles the opponent's AC, deal double damage. Same for triples, quadruples, etc.
    If you roll a 1, perform same steps, but subtract the 1d10-1 results from the attack roll.
  -}

  -- Roll the d10-1s, possibly multiple times
  perrinCrit :: Roll
  perrinCrit = StateT $ \h -> do { r <- liftIO $ rand (0,9)
                                 ; if r /= 9 then return (r, addR 0 r h)
                                   else do (newV, newH) <- runStateT perrinCrit (addR 0 r h)
                                           return (r + newV, newH)
                                 }

  -- Roll a s-sided die, given a critical range cr. e.g rollDieCritical 20 [18,19,20]
  rollDieCritical :: Int -> CritRange -> Roll
  rollDieCritical s cr = StateT $ \h -> do { (r,newH) <- runStateT (rollDie s) h
                                           ; if r `elem` cr then do (v,finalH) <- runStateT perrinCrit newH
                                                                    return (r + v, finalH)
                                             else return (r,newH)
                                           }


  pp :: IO String -> IO()
  pp s = s >>= putStrLn

  -- Pretty Print a roll
  ppRoll :: Roll -> IO String
  ppRoll s = do (v, h) <- runStateT s []
                return $ (ppRollHistory $ reverse h) ++ show v

  ppRollHistory :: RollHistory -> String
  ppRollHistory h = (concatMap ppGroupedRollHistories $ groupPerrins h) ++ "\n"
    where ppGroupedRollHistories (r:rs) = "(" ++ show (snd r) ++ insides rs ++ ")"
          insides rs = concatMap (\s -> "," ++ show (snd s)) rs

  -- Group a critical roll with it's subsequent Perrin rolls
  groupPerrins :: RollHistory -> [RollHistory]
  groupPerrins = groupBy (\_ (s2,_) -> s2 == "Perrin")


  -- Die rolls, e.g 2 `d` 6 rolls a 6 sided die twice and sums the results
  d :: Int -> Int -> Roll
  n `d` s | n > 1 = StateT $ \h -> do { (r,newH) <- runStateT (rollDie s) h
                                      ; (restR, restH) <- runStateT ((n-1) `d` s) newH
                                      ; return (r + restR, restH)
                                      }
  n `d` s = StateT $ \h -> runStateT (rollDie s) h

  -- d, but using the Perrin critical system. Assumes s is the (only) critical roll
  dc :: Int -> Int -> Roll
  n `dc` s | n > 1 = StateT $ \h -> do { (r,newH) <- runStateT (rollDieCritical s [s]) h
                                       ; (restR, restH) <- runStateT ((n-1) `dc` s) newH
                                       ; return (r + restR, restH)
                                       }
  n `dc` s = StateT $ \h -> runStateT (rollDieCritical s [s]) h

  plus :: IO Int -> IO Int -> IO Int
  plus = liftM2 (+)

  minus :: IO Int -> IO Int -> IO Int
  minus = liftM2 (-)

  equals :: IO Int -> IO Int -> IO Bool
  equals = liftM2 (==)

  mult :: IO Int -> IO Int -> IO Int
  mult = liftM2 (*)

  -- io version of d
  ioD :: IO Int -> IO Int -> Roll
  ioN `ioD` ioS = do s <- liftIO ioS
                     n <- liftIO ioN
                     n `d` s

  -- io version of dc
  ioDc :: IO Int -> IO Int -> Roll
  ioN `ioDc` ioS = do s <- liftIO ioS
                      n <- liftIO ioN
                      n `dc` s

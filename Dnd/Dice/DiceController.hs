module Dnd.Dice.DiceController where
  import Dnd.Dice.DiceRoller (d, dc, Roll, RollHistory, ppRollHistory)
--  import Text.ParserCombinators.Parsec
  import Control.Monad.State.Lazy

  data Expr = Value Int
            | Add Expr Expr
            | Sub Expr Expr
            | Mult Expr Expr
            | D Expr Expr
            | Dc Expr Expr

  type DiceResult = StateT [RollHistory] IO Int

  eval :: Expr -> DiceResult
  eval (Value i) = return i
  eval (Add e1 e2) = performOnDiceResult (+) e1 e2
  eval (Sub e1 e2) = performOnDiceResult (-) e1 e2
  eval (Mult e1 e2) = performOnDiceResult (*) e1 e2
  eval (D e1 e2) = rollToDiceResult (d) e1 e2
  eval (Dc e1 e2) = rollToDiceResult (dc) e1 e2

  -- Perform the provided binary operation on two expressions. Concatinates their HistoryLists
  performOnDiceResult :: (Int -> Int -> Int) -> Expr -> Expr -> DiceResult
  performOnDiceResult op e1 e2 = StateT $ \h -> do (leftV, leftH) <- runStateT (eval e1) []
                                                   (rightV, rightH) <- runStateT (eval e2) []
                                                   return (leftV `op` rightV, leftH ++ rightH)

  -- Roll the provided roller on two expressions. Gets the new history and sticks it in
  rollToDiceResult :: (Int -> Int -> Roll) -> Expr -> Expr -> DiceResult
  rollToDiceResult op e1 e2 = StateT $ \h -> do (leftV, leftH) <- runStateT (eval e1) []
                                                (rightV, rightH) <- runStateT (eval e2) []
                                                (midV, midH) <- runStateT (leftV `op` rightV) []
                                                return (midV, leftH ++ (midH : rightH))

  ppDiceResult :: DiceResult -> IO ()
  ppDiceResult dr = do (v, hs) <- runStateT dr []
                       putStrLn $ concatMap (ppRollHistory . reverse) hs
                       putStrLn $ show v

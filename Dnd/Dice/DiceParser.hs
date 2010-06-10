module Dnd.Dice.DiceParser where
  import Dnd.Dice.DiceController
  import Data.Char
  import Data.List
  import Data.List.Utils

  -- After trying parsec for a while, I'm just going to handroll a custom parser
  -- This is not code that I'm particularly happy with. I'll come back around to fixing this later

  {-- Grammar:
    Stmt := Term {('+'|'-') Stmt}
    Term := Factor {'*' Term}
    Factor := Expr {('d'|'dc') Expr}      -- something like 4d6d20 is not allowed, use (4d6)d20
    Expr := Number | 'p' | '(' Stmt ')'
  --}

  type Tokens = [String]

  -- Parse the input, spit out an Expr
  parser :: String -> Expr
  parser = parseStmt . lexer

  -- Custom lexer, remove spaces and break the string apart into tokens
  lexer :: String -> Tokens
  lexer [] = []
  lexer s = breakItDown $ filter (not . isSpace) s
    where breakItDown ('(':rest) = "LParen" : lexer rest
          breakItDown (')':rest) = "RParen" : lexer rest
          breakItDown ('c':'r':'i':'t':rest) = "Dc" : lexer rest
          breakItDown ('c':rest) = "Dc" : lexer rest
          breakItDown ('d':rest) = "D" : lexer rest
          breakItDown ('p':'e':'r':'r':'i':'n':rest) = "P" : lexer rest                                   
          breakItDown ('p':rest) = "P" : lexer rest
          breakItDown ('*':rest) = "Mult" : lexer rest
          breakItDown ('+':rest) = "Add" : lexer rest
          breakItDown ('-':rest) = "Sub" : lexer rest
          breakItDown (i:rest) | isDigit i = ("Value " ++ [i] ++ otherNumbers) : lexer remaining
            where (otherNumbers, remaining) = span isDigit rest
          breakItDown (e:errs) = error $ "Lexing error at " ++ [e] ++ ", rest of input is: " ++ errs


  -- The functions below parse their respective productions, as outlined in the grammar above
  -- Note that the existance of parenthesis adds a small caveat, as we can't just grab operators
  -- from anywhere, only from the top level, i.e. when they are not enclosed in any set of parenthesis
  -- The functions existsAtTop and topBreak handle this caveat for us.

  parseStmt :: Tokens -> Expr
  parseStmt ts | existsAtTop ["Add", "Sub"] ts = (constructor middle) (parseTerm left) (parseStmt right)
    where (left,middle,right) = topBreak ["Add", "Sub"] ts
          constructor "Add" = Add
          constructor "Sub" = Sub
  parseStmt ts = parseTerm ts

  parseTerm :: Tokens -> Expr
  parseTerm ts | existsAtTop ["Mult"] ts = Mult (parseFactor left) (parseTerm right)
    where (left, _, right) = topBreak ["Mult"] ts
  parseTerm ts = parseFactor ts

  parseFactor :: Tokens -> Expr
  parseFactor ts | existsAtTop ["D","Dc"] ts = (constructor middle) (parseExpr left) (parseExpr right)
    where (left,middle,right) = topBreak ["D", "Dc"] ts
          constructor "D" = D
          constructor "Dc" = Dc
  parseFactor ts = parseExpr ts

  parseExpr :: Tokens -> Expr
  parseExpr [t] | startswith "Value " t = Value . read $ drop 6 t    -- a little hackish
  parseExpr ["P"] = P
  parseExpr ("LParen":ts) = if last ts == "RParen" then parseStmt (dropLast ts)
                            else error "Unbalanced Parenthesis"
    where dropLast xs = take (length xs - 1) xs
  parseExpr xs = error $ concat xs

  -- See if any of the given tokens is at the top level, i.e. not enclosed in parens
  existsAtTop :: Tokens -> Tokens -> Bool
  existsAtTop ss ts = case topBreak ss ts of (_,"",_) -> False
                                             _ -> True

  -- Break at the first occurance of an element of ss, when that element is currently on top.
  -- Returns (prefix, element, suffix) 
  -- Note that, unlike break, the element itself is the middle result instead of being on the suffix
  -- Assumes that ss doesn't contain a paren
  topBreak :: Tokens -> Tokens -> (Tokens, String, Tokens)
  topBreak ss ts = topBreakWDepth ss ts 0 []
    where topBreakWDepth ss ("RParen":ts) n h = topBreakWDepth ss ts (n-1) ("RParen":h)
          topBreakWDepth ss ("LParen":ts) n h = topBreakWDepth ss ts (n+1) ("LParen":h)
          topBreakWDepth ss (t:ts) 0 h | t `elem` ss = (reverse h,t,ts)
          topBreakWDepth ss (t:ts) n h = topBreakWDepth ss ts n (t:h)
          topBreakWDepth _ [] _ h = (reverse h, "", [])

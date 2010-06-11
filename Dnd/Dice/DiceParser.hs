module Dnd.Dice.DiceParser where
  import Dnd.Dice.DiceController
  import Data.Char
  import Data.List
  import Data.List.Utils

  -- After trying parsec for a while, I'm just going to handroll a custom parser
  -- This is not code that I'm particularly happy with. I'll come back around to fixing this later

  -- We might turn a result, or we might turn an error
  data ParseEither a = Result a | Error String
                     deriving (Show, Eq, Read)

  -- Propagate errors, otherwise keep the results flowing
  instance Monad ParseEither where
                           return = Result
                           Error s >>= _ = Error s
                           Result a >>= f = f a
                           
  type Tokens = [String]

  type LexResult = ParseEither Tokens

  type ParseResult = ParseEither Expr

  -- | Bind 2 arguments to a function, and inject the result back into m
  binj :: Monad m => (a -> b -> c) -> m a -> m b -> m c
  binj f a b = do { left <- a ; right <- b ; return (f left right) }

  {-- Grammar:
    Stmt := Term {('+'|'-') Stmt}
    Term := Factor {'*' Term}
    Factor := Expr {('d'|'crit') Expr}      -- something like 4d6d20 is not allowed, use (4d6)d20
    Expr := Number | 'perrin' | '(' Stmt ')'
  --}

  -- | Parse the input, spit out an Expr
  parser :: String -> ParseResult
  parser s = lexer s >>= parseStmt 

  -- | Custom lexSer, remove spaces and break the string apart into tokens. On Failure, return Left (errorMessage)
  lexer :: String -> LexResult
  lexer s = lexStr (filter (not . isSpace) s)

  -- | Lex the string. If we encounter an error, just return an Error message, which is passed up the chain
  lexStr :: [Char] -> ParseEither [[Char]]
  lexStr ('(':rest)                     = lexStr rest >>= return . (:) "LParen"
  lexStr (')':rest)                     = lexStr rest >>= return . (:) "RParen"
  lexStr ('c':'r':'i':'t':rest)         = lexStr rest >>= return . (:) "Crit"
  lexStr ('c':rest)                     = lexStr rest >>= return . (:) "Crit"
  lexStr ('d':rest)                     = lexStr rest >>= return . (:) "D"
  lexStr ('p':'e':'r':'r':'i':'n':rest) = lexStr rest >>= return . (:) "Perrin"
  lexStr ('p':rest)                     = lexStr rest >>= return . (:) "Perrin"
  lexStr ('*':rest)                     = lexStr rest >>= return . (:) "Mult"
  lexStr ('+':rest)                     = lexStr rest >>= return . (:) "Add"
  lexStr ('-':rest)                     = lexStr rest >>= return . (:) "Sub"
  lexStr (i:more) | isDigit i           = lexStr rest >>= return . (:) ("Value " ++ [i] ++ otherNumbers)
    where (otherNumbers, rest) = span isDigit more
  lexStr (e:errs) = Error $ "Lexing error at " ++ [e] ++ ", rest of input is: " ++ errs
  lexStr [] = return []


  -- The functions below parse their respective productions, as outlined in the grammar above
  -- Note that the existance of parenthesis adds a small caveat, as we can't just grab operators
  -- from anywhere, only from the top level, i.e. when they are not enclosed in any set of parenthesis
  -- The functions existsAtTop and topBreak handle this caveat for us.

  -- parseStmt :: Tokens -> ParseResult
  parseStmt ts | existsAtTop ["Add", "Sub"] ts = binj (constructor middle) (parseTerm left) (parseStmt right)
    where (left,middle,right) = topBreak ["Add", "Sub"] ts
          constructor :: String -> (Expr -> Expr -> Expr)
          constructor "Add" = Add
          constructor "Sub" = Sub
  parseStmt ts = parseTerm ts

  -- parseTerm :: Tokens -> ParseResult
  parseTerm ts | existsAtTop ["Mult"] ts = binj Mult (parseFactor left) (parseTerm right)
    where (left, _, right) = topBreak ["Mult"] ts
  parseTerm ts = parseFactor ts

  -- parseFactor :: Tokens -> Expr
  parseFactor ts | existsAtTop ["D","Crit"] ts = binj (constructor middle) (parseExpr left) (parseExpr right)
    where (left,middle,right) = topBreak ["D", "Crit"] ts
          constructor "D" = D
          constructor "Crit" = Crit
  parseFactor ts = parseExpr ts

  -- parseExpr :: Tokens -> Expr
  parseExpr [t] | startswith "Value " t = return . Value . read $ drop 6 t    -- a little hackish
  parseExpr ["Perrin"] = return Perrin
  parseExpr ("LParen":ts) = if last ts == "RParen" then parseStmt (dropLast ts)
                            else Error "Parse error: Unbalanced Parenthesis"
    where dropLast xs = take (length xs - 1) xs
  parseExpr xs = Error $ "Parse error: unknown parse error on " ++ concat xs

  -- | See if any of the given tokens is at the top level, i.e. not enclosed in parens
  existsAtTop :: Tokens -> Tokens -> Bool
  existsAtTop ss ts = case topBreak ss ts of (_,"",_) -> False
                                             _ -> True

  -- | Break at the first occurance of an element of ss, when that element is currently on top.
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

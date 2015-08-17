module Parser where


--- Parser with operations, with the right precedence as explained in the module AbstractSyntax.



import Data.Char
import AbstractSyntax

data Token = ConstantT Int 
           | OpT OpName
           | IdT Identifier
           | OpenBracketT
           | CloseBracketT
           | AssignT          -- The token for the assignment symbol ":=".
           | IfT 
           | ElseT 
           | WhileT
           | OpenCurlyT
           | CloseCurlyT
           | SemicolonT
           deriving (Eq, Show)

keywordTable :: [(String, Token)]
keywordTable = [("while", WhileT), 
                ("if", IfT), 
                ("else",ElseT)]

identifierOrKeyWord :: String -> Token
identifierOrKeyWord xs = 
  case lookup xs keywordTable of 
    Nothing -> IdT xs
    Just t  -> t

operatorTable :: [(String, Token)]
operatorTable = [("|", OpT Or),
		("&", OpT And),
		("==", OpT Eq), 
                 ("<=", OpT Leq),
                 ("<", OpT Less),
                 (">=", OpT Geq),
                 (">", OpT Greater),
                 ("+", OpT Add),    
                 ("-", OpT Sub),
                 ("*", OpT Mul), 
                 ("/", OpT Div),
                 ("%", OpT Mod),
                 ("!", OpT Not),
                 (":=", AssignT)] 

operatorLookup :: String -> Token
operatorLookup xs = 
  case lookup xs operatorTable of 
    Nothing -> lexicalError("Unexistent operator " ++  xs)
    Just t  -> t

opDelimiter :: Char -> Bool                                 
opDelimiter c = isSpace c || isDigit c || isAlpha c || c `elem` ['(', ')' , '{' , '}', ';']

operator :: Char -> Bool
operator c = not(opDelimiter c)

opEnd :: String -> Bool
opEnd [] = True
opEnd (x:xs) = opDelimiter x
                                 
assumeOpEnd :: String -> String
assumeOpEnd xs = if opEnd xs then xs else lexicalError ("Ill-formed end of operator: " ++ show xs)
  
constantDelimiter :: Char -> Bool                                 
constantDelimiter c = isSpace c || c `elem` ['(', ')' , '{' , '}', ';']
                   
constantEnd :: String -> Bool
constantEnd [] = True
constantEnd (x:xs) = constantDelimiter x
                      
                      
lexicalError :: String -> a                   
lexicalError xs = error ("Lexical error: " ++ xs)
                  
lexicalAnalysis :: String -> [Token]

lexicalAnalysis [] = []

lexicalAnalysis ('(':xs) = OpenBracketT : lexicalAnalysis xs
lexicalAnalysis (')':xs) = CloseBracketT: lexicalAnalysis xs
lexicalAnalysis ('{':xs) = OpenCurlyT   : lexicalAnalysis xs
lexicalAnalysis ('}':xs) = CloseCurlyT  : lexicalAnalysis xs
lexicalAnalysis (';':xs) = SemicolonT   : lexicalAnalysis xs

lexicalAnalysis (x:xs) -- Identifiers or keywords start with alphabetical character.
           | isAlpha x = let (ys, zs) = span (\x -> isAlpha x || isDigit x || x == '_') xs
                         in identifierOrKeyWord (x:ys) : lexicalAnalysis zs

lexicalAnalysis (x:xs) -- We don't have negative constants in this version (have to write 0 - 5 instead of -5).
           | isDigit x = ConstantT n : lexicalAnalysis zs
           where 
             (ys, zs) = span isDigit xs
             n = if constantEnd zs then read(x:ys) else lexicalError zs

lexicalAnalysis (x:xs) -- Ignore all kinds of spaces (whitespace, tab, end of line).
           | isSpace x = lexicalAnalysis xs

lexicalAnalysis xs     -- Assume it is an operator.
           | otherwise = let (ys, zs) = span operator xs
                         in operatorLookup ys : lexicalAnalysis zs




{- Grammar 

Program ::= Variable := Expr; 
          | If (Expr) Program 
          | If (Expr) Program else Program | 
          | while (Expr) Program
          | { [Program] }
          
OrExp ::= AndExp | AndExp OrOp AndExp
AndExp ::= EqExp | EqExp AndOp EqExp
EqExp ::= Expr | Expr EqOp Expr
Expr ::= Arith | Arith ComparisonOp Expr   
Arith ::= Term | Term AddOp Arith
Term ::= FactorNot | FactorNot MulOp FactorNot
FactorNot := Factor | NotOp FactorNot
Factor := Constant | Variable | (OrExp) 

OrOp := |
AndOp := &
EqOp := ==
ComparisonOp = >, >=, <=, <
AddOp := + | -
MulOp := * | / | %
NotOp := !

-}

--- Functions matching the grammar described above

syntacticalAnalysis :: [Token] -> Program
scanProgram :: [Token] -> (Program, [Token])
scanProgramList :: [Token] -> ([Program], [Token])
scanOrExp, scanAndExp, scanEqExp, scanArith, scanExpr, scanTerm, scanFactorNot, scanFactor :: [Token] -> (Expr, [Token])

syntacticalAnalysis xs = 
  let 
    (t, ys) = scanProgram xs 
  in 
   case ys of
     [] -> t 
     _  -> syntaxError("Unexpected program tail " ++ show ys) 

scanProgram (IdT i : xs) = (i := e, ws)
  where
    ys = require AssignT xs
    (e , zs) = scanOrExp ys
    ws = require SemicolonT zs
    
scanProgram (IfT : xs) =
  let
    ys = require OpenBracketT xs
    (e, zs) = scanOrExp ys
    ws = require CloseBracketT zs
    (p, ts) = scanProgram ws 
  in
   case ts of
     (ElseT : us) -> let (q, vs) = scanProgram us
                     in (IfThenElse e p q, vs)
     []           -> (IfThen e p, [])
     us           -> (IfThen e p, us)                    

scanProgram (WhileT : xs) = (While e p, ts)
  where
    ys = require OpenBracketT xs
    (e, zs) = scanOrExp ys
    ws = require CloseBracketT zs
    (p, ts) = scanProgram ws 

scanProgram (OpenCurlyT : xs) = (Block ps, ys)
  where
   (ps, ys) = scanProgramList xs

scanProgram xs = syntaxError("Unrecognized program: " ++ show xs)


scanProgramList [] = syntaxError("Expected '}' but found end of input")

scanProgramList (CloseCurlyT : xs) = ([], xs)

scanProgramList xs = (p : ps, zs)
  where
    (p, ys) = scanProgram xs
    (ps, zs) = scanProgramList ys

scanOrExp xs = 
  let (t, ys) = scanAndExp xs
  in case ys of    
       OpT o : zs -> if o == Or
                     then 
                       let (u, ws) = scanOrExp zs in (Op o [t, u], ws)
                     else 
                       (t, ys)
       
       _          -> (t, ys)
       
scanAndExp xs = 
  let (t, ys) = scanEqExp xs
  in case ys of    
       OpT o : zs -> if o == And
                     then 
                       let (u, ws) = scanAndExp zs in (Op o [t, u], ws)
                     else 
                         (t, ys)
       
       _          -> (t, ys)
       
scanEqExp xs = 
  let (t, ys) = scanExpr xs
  in case ys of    
       OpT o : zs -> if o == Eq
                     then 
                       let (u, ws) = scanEqExp zs in (Op o [t, u], ws)
                     else 
                        (t, ys)
       
       _          -> (t, ys)       
            
scanExpr xs = 
  let (t, ys) = scanArith xs
  in case ys of    
       OpT o : zs -> if o == Leq || o == Less||   o == Geq  || o ==  Greater 
                     then 
                       let (u, ws) = scanExpr zs in (Op o [t, u], ws)
                     else 
                       (t, ys)
       
       _          -> (t, ys)
       
scanArith xs = 
  let (t, ys) = scanTerm xs
  in case ys of    
       OpT o : zs -> if o == Add || o == Sub 
                     then 
                       let (u, ws) = scanArith zs in (Op o [t, u], ws)
                     else 
                       (t, ys)
       
       _          -> (t, ys)

scanTerm xs = 
  let (t, ys) = scanFactorNot xs
  in case ys of    
       OpT o : zs -> if o == Mul || o == Div || o == Mod
                     then 
                       let (u, ws) = scanTerm zs in (Op o [t, u], ws)
                     else 
                       (t, ys)
       
       _          -> (t, ys)

scanFactorNot xs = 
  let (t, ys) = scanFactor xs
  in case xs of    
       OpT o : zs -> if o == Not
                     then 
                       let (u, ws) = scanFactorNot zs in (Op o [u], ws)
                     else 
                       (t, ys)
       
       _          -> (t, ys)

scanFactor (ConstantT n : xs) = (Constant n,  xs)
scanFactor (IdT n : xs) = (Var n,  xs)  
scanFactor (OpenBracketT : xs) = 
  let (t , ys) = scanOrExp xs
  in (t, require CloseBracketT ys)
scanFactor [] = syntaxError("Unexpected end of input")           
scanFactor xs = syntaxError("Unexpected " ++ show xs)      

require :: Token -> [Token] -> [Token]
require t [] = syntaxError ("Expected " ++ show t ++ " but found end of input")
require t (x : xs) 
  | t == x    = xs
  | otherwise = syntaxError("Expected " ++ show t ++ " but found " ++ show(x:xs))

syntaxError xs = error ("Syntax error: " ++ xs)
syntaxError :: String -> a                   

parseProgram :: String -> Program
parseProgram = syntacticalAnalysis . lexicalAnalysis    

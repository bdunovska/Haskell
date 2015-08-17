module Solution where

{-

A functional interpreter for a minimal imperative language.

   Arithmetical operators +, -, *, /
   Comparisons =, <, <=, >, >=
   Logical operators and, or not.
   Assignment statements variable := expression
   If-then-else statements
   While statements
   Block statements
 
We have integers variables only, we ignore errors such as undefined
variables and division by zero, and we work with abstract syntax (the
types OpName, Expr and Program defined below).  
   
-}

type Identifier = String

data OpName = Cond
            | Add
            | Sub
            | Mul
            | Div
            | Neg
            | Eq
            | Leq
            | Less
            | Geq
            | Greater
            | And
            | Or
            | Not
            deriving (Eq, Show)

data Expr = Constant Int
          | Var Identifier
          | Op OpName [Expr]
          deriving (Eq, Show)

data Program = Identifier := Expr
             | IfThenElse Expr Program Program
             | While Expr Program
             | Block [Program]
             deriving (Eq, Show)

skip :: Program
skip = Block []    -- does nothing.

ifThen :: Expr -> Program -> Program
ifThen e p = IfThenElse e p skip

{-
Similar to the above, definition of a for-loop program constructor by reduction to while-loops.
-}

for :: Program -> Expr -> Program -> Program -> Program
for init test update body = Block [init, While test (Block [body,update])]

{-

    In concrete syntax

            for (init; test; update)
                body;

    should be equivalent to

            init;
            while (test)
              {
                body;
                update;
              }
-}

{-

Example. A program for calculating factorial, with the
convention that the input is x and the output is y.


  {
   y := 1;
   for (n := x; n > 0; n := n - 1)
     y := y * n;
  }

Program in abstract syntax (using our "for" function):

-}

factorial :: Program
factorial = for (Block ["y" := (Constant 1), "n" := Var "x"]) (Op Greater [Var "n",Constant 0]) ("n" := (Op Sub [Var "n", Constant 1])) ("y" := Op Mul [Var "n", Var "y"])


code :: Bool -> Int
code False = 0
code True  = 1

decode :: Int -> Bool
decode 0 = False
decode _ = True

opeval :: OpName -> [Int] -> Int
opeval Add     [x, y] = x + y
opeval Sub     [x, y] = x - y
opeval Mul     [x, y] = x * y
opeval Div     [x, y] = x `div` y
opeval Neg     [x]    = - x
opeval Eq      [x, y] = code(x == y)
opeval Leq     [x, y] = code(x <= y)
opeval Less    [x, y] = code(x < y)
opeval Geq     [x, y] = code(x >= y)
opeval Greater [x, y] = code(x > y)
opeval And     [x, y] = code(decode x && decode y)
opeval Or      [x, y] = code(decode x || decode y)
opeval Not     [x]    = code(not(decode x))
opeval Cond    [x,y,z] = if decode x then y else z
opeval op      xs     = error ("Tried to apply " ++ (show op) ++ " to " ++ show xs)

type Memory = Identifier -> Int

mExample :: Memory
mExample "x" = 3
mExample "y" = 4
mExample "z" = 5
mExample _   = undefined

eval :: Memory -> Expr ->  Int
eval m (Constant x) = x
eval m (Var i)      = m i
eval m (Op o es)    = opeval o [eval m e | e <- es]

write :: Identifier -> Int -> Memory -> Memory
write i x m = m'
 where
   m' :: Memory
   m' j
     | i == j    = x
     | otherwise = m j

run :: Program -> Memory -> Memory

run (i := e) m = write i (eval m e) m

run (IfThenElse e p q) m
    | decode(eval m e) = run p m
    | otherwise        = run q m

run (While e p) m
    | decode(eval m e) = m''
    | otherwise        = m
    where
      m'  = run p m
      m'' = run (While e p) m'

run (Block []) m = m

run (Block (p : ps)) m = m''
    where
      m'  = run p m
      m'' = run (Block ps) m'

runxy :: Program -> Int -> Int
runxy p x = m' "y"
 where
   m :: Memory
   m "x" = x
   m i   = error ("Non-existent memory location " ++ i)

   m' :: Memory
   m' = run p m

{-

Example, continued from the above (you can use this to test your solutions):

-}

fact :: Int -> Int
fact = runxy factorial

{-

There is another way to represent memory:

-}

type Memory' = [(Identifier,Int)]


--- New definition of a memory

mExample' :: Memory'
mExample' = [("x", 3), ("y", 4), ("z", 5)]

read' :: Identifier -> Memory' -> Int
read' i m = case m of
	[] -> error "empty"
	(x,y):xs -> if i==x then y else read' i xs 

eval' :: Memory' -> Expr ->  Int
eval' m (Constant x) = x
eval' m (Var i)      = read' i m 
eval' m (Op o es)    = opeval o [eval' m e | e <- es]

write' :: Identifier -> Int -> Memory' -> Memory'
write' i x m = case m of
			[] -> [(i,x)]
			_ -> (i,x):m

run' :: Program -> Memory' -> Memory'

run' (i := e) m = write' i (eval' m e) m

run' (IfThenElse e p q) m
    | decode(eval' m e) = run' p m
    | otherwise        = run' q m

run' (While e p) m
    | decode(eval' m e) = m''
    | otherwise        = m
    where
      m'  = run' p m
      m'' = run' (While e p) m'

run' (Block []) m = m

run' (Block (p : ps)) m = m''
    where
      m'  = run' p m
      m'' = run' (Block ps) m'

runxy' :: Program -> Int -> Int
runxy' p x =  read' "y" m'
	where
         m::Memory'
	 m = [("x",x)]
	 
	 m'::Memory'
	 m' = run' p m


fact' :: Int -> Int
fact' = runxy' factorial

{-

Simple program optimization. Say that an expression is constant if it
doesn't use variables. We can replace constant subexpresions by their
values, as follows:

-}

replaceConstantSubexpressions :: Expr ->  Expr
replaceConstantSubexpressions (Constant x) = Constant x
replaceConstantSubexpressions (Var i)      = Var i
replaceConstantSubexpressions (Op o es)
               | all isConstant vs = Constant(opeval o [n |  Constant n <- vs])
               | otherwise         = Op o vs
               where
                 vs = [replaceConstantSubexpressions e | e <- es]

isConstant :: Expr -> Bool
isConstant (Constant _) = True
isConstant _ = False

{-

Similarly, if an IfThenElse program has
its condition constant, it can be replaced by a simpler program (one
of the two branches). The same applies to while loops (when the
condition is false). Moreover, we can apply the previous optimization
to every subexpression in a program.

-}

eval1 ::  Expr ->  Int
eval1 (Constant x) = x

constantFolding :: Program -> Program
constantFolding (i := e) =  i:=replaceConstantSubexpressions e
constantFolding (IfThenElse e x y) |isConstant  (replaceConstantSubexpressions e) = if decode (eval1 (replaceConstantSubexpressions e)) then constantFolding x else constantFolding y
                                   |otherwise = IfThenElse (replaceConstantSubexpressions e) (constantFolding x) (constantFolding y)
constantFolding (While e x) |isConstant (replaceConstantSubexpressions e) =  if decode (eval1 (replaceConstantSubexpressions e)) then (While (replaceConstantSubexpressions e) (constantFolding x) ) else skip
			    |otherwise = (While (replaceConstantSubexpressions e) (constantFolding x) )
constantFolding (Block (x)) = Block [constantFolding e | e <- x]
 

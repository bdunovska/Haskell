module Interpreter where

import Control.Monad
import AbstractSyntax
            
--- A functional interpreter for a minimal imperative language.            
                     
code :: Bool -> Int
code False = 0
code True  = 1

decode :: Int -> Bool
decode 0 = False  
decode _ = True

opeval :: MonadPlus m => OpName -> [Int] -> m Int
opeval Add     [x, y] = return (x + y)
opeval Sub     [x, y] = return (x - y)
opeval Mul     [x, y] = return (x * y)
opeval Div     [x, y] = guard(y/=0)>>return (x `div` y) 
opeval Mod     [x, y] = guard(y/=0)>>return (x `mod` y) 
opeval Eq      [x, y] = return (code(x == y))
opeval Leq     [x, y] = return (code(x<=y))
opeval Less    [x, y] = return (code(x<y))
opeval Geq     [x, y] = return (code(x >= y))
opeval Greater [x, y] = return (code(x >y))
opeval And     [x, y] = return (code(decode x && decode y))
opeval Or      [x, y] = return (code(decode x || decode y))
opeval Not     [x]    = return (code(not(decode x)))
opeval op      xs     = error ("Tried to apply " ++ (show op) ++ " to " ++ show xs)



type Memory m = Identifier -> m Int
           
memExample :: MonadPlus m => Memory m
memExample "x" = return 3
memExample "y" = return 4
memExample "z" = return 5
memExample _ = mzero  

eval :: MonadPlus m => Memory m -> Expr ->  m Int
eval m (Constant x) = return x
eval m (Var i) = m i  
eval m (Op o l) =  do
		   x<-mapM (eval m) l
 		   opeval o x
 		   
write :: MonadPlus m => Identifier -> Int -> Memory m -> Memory m
write i x m j = if i == j then (return x) else m j	

run :: MonadPlus m => Program -> Memory m -> m(Memory m)

run (i := e) mem = do 
	x<-eval mem e 
	return (write i x mem)

run (IfThenElse e p q) mem =
    do x <- eval mem e
       if decode x 
          then run p mem
          else run q mem
               
run (IfThen e p) mem  = 
	do x <- eval mem e
           if decode x 
             then run p mem
             else return mem
              
run (While e p) mem  = 
	do x <- eval mem e
           if decode x 
             then  let m'=run p mem in
		m' >>= (\x-> run (While e p) x)
             else return mem

run (Block []) mem = return mem

run (Block (p : ps)) mem = 
	do 
		mem'<- run p mem 
	 	run (Block ps) mem'

runxy :: MonadPlus m => Program -> Int -> m Int
runxy p x = do mem' <- run p mem
               mem' "y"
 where
   mem "x" = return x
   mem _   = mzero

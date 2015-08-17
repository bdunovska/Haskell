module Solution where
import Data.Monoid

{-
The identity monad. Does nothing!

-}

data Id a = Box a deriving Show

instance Monad Id where
  return a = Box a
  (Box a) >>= f = f a


--- There is no need to use a monad, but we can:
mfib :: Monad m => Integer -> m Integer
mfib n =
  if n == 0 || n == 1
         then return n
         else do x <- mfib(n-1)
                 y <- mfib(n-2)
                 return(x + y)


fact :: Monad m => Integer -> m Integer
fact n = if n == 0 then return 1 else do 
					x<-fact(n-1)
					return (n*x)

-- Without using the monad, mfib amounts to:

fib :: Integer -> Integer
fib n =
  if n == 0 || n == 1
         then n
         else fib(n-1) + fib(n-2)


{-

The above function computes in exponential time.

-}

fib' :: Integer -> Integer
fib' n = f n 0 1
  where
    f 0 a b = a
    f n a b = f(n-1) b (a+b)

{- This is linear time because there is one recursive call only. -}

{-

Let's trace the execution of fib by "printing" while we compute.  But
without the IO monad.

We use our own writer monad.

-}

data Writer a = Writer a String deriving Show

instance Monad Writer where
  return a = Writer a []
  (Writer a xs) >>= f = Writer b (xs ++ ys)
    where
      Writer b ys = f a

tell :: String -> Writer ()
tell xs = Writer () xs

wfib :: Integer -> Writer Integer
wfib n = do
  tell (" call with " ++ show n ++ ", ")
  if n == 0 || n == 1
         then return n
         else do x <- wfib(n-1)
                 y <- wfib(n-2)
                 return(x + y)

wfib' :: Integer -> Writer Integer
wfib' n =
  tell (" call with " ++ show n ++ ", ") >>
  (if n == 0 || n == 1
         then return n
         else wfib(n-1) >>= (\x -> wfib(n-2) >>= (\y -> return(x + y))))


wfact :: Integer -> Writer Integer
wfact n = 
	tell (" call with number " ++ show n ++ " ... " ) >>
	if n == 0 then return 1 else wfact (n - 1) >>=(\x-> return (n*x)) 
	
wfact' n = do 
	tell (" call with number " ++ show n ++ " ... " )
	if n == 0 then return 1
	else do x<-wfact' (n-1)
		return(n*x)
{-

We can use a similar monad to keep track of the number of recursive calls.

-}

data Counter a = Counter a Integer deriving (Eq, Show)

instance Monad Counter where
  return a = Counter a 0
  (Counter a x) >>= f = Counter b (x + y)
    where
      Counter b y = f a

count :: Counter ()
count = Counter () 1

cfib :: Integer -> Counter Integer
cfib n = do
  count
  if n == 0 || n == 1
         then return n
         else do x <- cfib(n-1)
                 y <- cfib(n-2)
                 return(x + y)
                 
cfib' n = count>> if( n == 0 || n == 1) then return n else cfib'(n-1) >>= (\x-> (cfib'(n-2) >>= (\y -> return (x + y))))                 
                 

--- Version of the factorial function that counts the number of calls.
cfact :: Integer -> Counter Integer
cfact n = do 
	 count 
	 if n == 0 then return 1 else do
	 		x<-cfact(n-1)
	 		return (x*n)

cfact' n = count>> if n == 0 then return 1 else cfact' (n-1) >>= (\x-> return (x*n))



data State s a = State(s -> (a,s))

runState :: State s a -> s -> (a, s)
runState (State f) = f

instance Monad (State s) where
  return x = State(\s -> (x,s))
  (State h) >>= f = State(\s -> let (a, s') = h s
                                    (State g) = f a
                                in  g s')

get :: State s s
get = State(\s -> (s,s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = State(\s -> ((), f s))

{-

In the following example, the state consists of one Integer, which we
use to count recursive calls. We modify the state by applying the
successor function. The other Integer is not the state, but rather the result of the function.

-}

sfibHelper :: Integer -> State Integer Integer
sfibHelper n = do
         modify succ
         if n == 0 || n == 1
          then return n
          else do x <- sfibHelper(n-1)
                  y <- sfibHelper(n-2)
                  return(x + y)


{- Returns the value of the fib function and the number of recursion unfoldings used: -}

sfib :: Integer -> (Integer , Integer)
sfib n = runState (sfibHelper n) 0

--- Instead of counting the number of recursive calls, record all arguments used in recursive calls in a list. 

sfib' :: Integer -> State [Integer] Integer 
sfib' n = do
         modify (\x->n:x)
         if n == 0 || n == 1
          then return n
          else do x <- sfib'(n-1)
                  y <- sfib'(n-2)
                  return(x + y)

--- This can be done in another way:

data Writer' w a = Writer' [w] a deriving (Eq, Show)

---

instance Monad (Writer' w) where
  return a =  Writer' [] a
  Writer' xs a >>= f = let Writer' ys b = f a	   
  	in 
  	Writer' (xs ++ ys) b
			
tell' :: [w] -> Writer' w ()
tell' xs = Writer' xs ()

--- Count recursion unfoldings:

wfib'' :: Integer -> Writer' Integer Integer
wfib'' n = do 
		tell' [n]
		if n == 0 || n == 1
		then return n 
		else do x<-wfib'' (n-1)
			y<-wfib'' (n-2)
			return (x+y)
			
{- Using a timeout monad, we can limit how long we are willing to wait. -}

data Timeout a = Timeout (Integer -> Maybe (a , Integer))
runTimeout :: Timeout a -> Integer -> Maybe (a, Integer)
runTimeout (Timeout x) = x


---

instance Monad Timeout where
  return a = Timeout(\x-> Just(a,x))
  (Timeout ma) >>= f = Timeout(\x-> if x == 0 then Nothing else 
  				case ma x of
  				Just (b,s) -> let   (Timeout g) = f b in g s
  				Nothing -> Nothing)
  				
tick :: Timeout ()
tick = Timeout(\x-> if x>0 then Just( (), x-1) else Nothing)

--- Here is an example of how this can be used:

tfib :: Integer -> Timeout Integer
tfib n = do
  tick
  if n == 0 || n == 1
    then return n
    else do
      x <- tfib (n - 1)
      y <- tfib (n - 2)
      return (x + y)

example1, example2 :: Maybe (Integer, Integer) -- Just (fib n, steps left) in case of success.
example1 = runTimeout (tfib 7) 50
example2 = runTimeout (tfib 8) 50

--- Combination of state monad and writer monad using a monoid. 

newtype WS w s a = WS (s -> (w, s, a))
runWS :: WS w s a -> s -> (w, s, a)
runWS (WS f) = f

instance Monoid w => Monad (WS w s) where
  return a = WS(\s->(mempty, s, a))
  WS f >>= g = WS(\s-> let (xs,s',b) = f s
  			   (WS g') = g b
  			   (ys,p,q)=  g' s'   
		           in ((mappend xs ys), p, q))	
		           
tellWS :: w -> WS w s ()
tellWS xs = WS(\s->(xs,s,()))

putWS :: Monoid w => s -> WS w s ()
putWS s = WS(\_->(mempty,s,()))

getWS :: Monoid w => WS w s s
getWS = WS(\s->(mempty, s,s))


---arithmetic expressions built up from integers, addition and multiplication

data Expr = Val Int | Add Expr Expr | Mul Expr Expr deriving (Show)


---represents (1 + 5) ∗ 3
example = Mul (Add (Val 1) (Val 5)) (Val 3)          
          
---evaluates an expression to its integer value          
eval::Expr -> Int
eval (Val x ) = x
eval (Add e e1) = eval e + eval e1
eval (Mul e e1) = eval e * eval e1

---returns all values in an expression
values::Expr -> [Int]
values (Val x) = [x]
values (Add e e1) = values e ++ values e1  
values (Mul e e1) = values e ++ values e1 

--- returns all ways of breaking a list into two non-empty parts that append to give the original list
break1::[a]->[([a],[a])]
break1 l = reverse (acc ((length l)-1) l )
	where
acc 1 l = [(take 1 l, drop 1 l)] 
acc m l = (p,q):acc (m-1) l
	where
	p = take m l
	q = drop (length p) l
	
--- all expressions whose list of values is precisely a given list	
exprs [] = [Val 0]
exprs [n] = [Val n]
exprs ns = [r | (xs,ys) <- break1 ns, x <-( exprs xs) , y <- (exprs ys), r <- [Add x y, Mul x y]]

---solutions ns n returns all expressions whose list of values is ns and whose evaluation is n.
solution::[Int]->Int->[Expr]
solution ns n = let exps = exprs ns 
	in [r|r<-exps, eval r == n]
	
---TREE

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show,Eq,Ord)

---creates a NodeTree
node:: a->Tree a
node x = Node x EmptyTree EmptyTree

---insertion
insert::(Ord a) => Tree a-> a-> Tree a
insert (Empty) x = Node x Empty Empty
insert (Node p z y) x= if x>p then Node p z (insert y x) else Node p (insert z x) y 

---checks whether it is a perfectly balanced tree
isPerfect::Tree a -> Bool
isPerfect (Empty) = True
isPerfect (Node x p q) = (count p) == (count q) && (isPerfect p) && (isPerfect p)
			where count::Tree a -> Int
			      count (Empty) = 0
			      count (Node x p q) = 1 + count p + count q 
			      
			      
---fold implementation for a tree
fold:: (a->b->a) -> a -> Tree b -> a
fold f a Empty = a
fold f a (Node x p q) = left  
			where right = fold f ( f a x )  q		      
			      left = fold f right p	

---check for an element in a tree          
isIn::(Eq a) => a->Tree a-> Bool
isIn x EmptyTree = False
isIn x (Node p a b) = if x == p then True else (isIn x a) || (isIn x b)  	

---deletiion
moveup :: (Eq a, Ord a) => Tree a -> Tree a			     
moveup Empty = Empty
moveup (Node x p q) = case (p,q) of
	(Empty, Empty) -> Empty
	(Empty, Node a b c) ->q
	(Node a b c, Empty) ->p	
	(Node a b c, Node o t s) -> (Node (leftmost q) p (delete (leftmost q) q))
				where leftmost (Node a Empty c) = a
				      leftmost (Node a b c) = leftmost b
	
delete :: (Eq a, Ord a) => a-> Tree a -> Tree a	
delete x Empty = Empty
delete x t@(Node a b c) = if x == a then moveup t else (Node a (delete x b) (delete x c))

---Alternative representation of a list
data List a = Nil | Cons a (List a)
     deriving (Eq, Show) 

--- The following functions showcase that this type is isomorphic to [a].
--- toNormal(fromNormal xs) = xs
--- fromNormal(toNormal xs) = xs

toNormal :: List a -> [a]
toNormal Nil = []
toNormal (Cons a b) = a:(toNormal b)

fromNormal :: [a] -> List a
fromNormal [] = Nil
fromNormal (x:xs) = Cons x ( fromNormal xs ) 



--- A "listShow" function for List a, to show it as (the more readable) "{3; 4; 5}". 

listShow :: Show a => List a -> String             
listShow Nil = "{}"
listShow (Cons a Nil) = "{" ++ (show a) ++ "}"
listShow (Cons a b) = "{" ++ (show a)  ++ listShow' b ++ "}"
			where 
			listShow' (Nil) = ""
			listShow' (Cons x y) = "; "  ++ show x  ++ listShow' y
  
--- map
listMap :: (a -> b) -> List a -> List b
listMap f Nil = Nil
listMap f (Cons a b) = Cons (f a) (listMap f b) 

--- filter
listFilter :: (a -> Bool) -> List a -> List a
listFilter p Nil = Nil
listFilter p (Cons a b) = if p a then (Cons a (listFilter p b)) else listFilter p b


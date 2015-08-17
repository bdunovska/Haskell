--- We compute the list of prime factors for each number.
--- Produces an infinite list. 
--- https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

sieve :: [[Int]]
sieve = sievefrom 2 (repeat [])
 
sievefrom :: Int -> [[Int]] -> [[Int]]
sievefrom n ([]  : xs) = []  : sievefrom (n+1) (cross n xs)
sievefrom n (x : xs) = x : sievefrom (n+1) xs

cross :: Int -> [[Int]] -> [[Int]] 
cross n xs = cross' (n-1) xs
  where 
    cross' 0 (x : xs) = (n : x) : cross' (n-1) xs
    cross' i (x : xs) = x     : cross' (i-1) xs

---the factors of the number n
factors :: Int -> [Int]
factors n = sieve !! (n-2)

---is n prime?
prime1 ::  Int -> Bool
prime1 n =  factors n == []



---returns the product of a list of integers
productList::[Int]->Int
productList [] = 1
productList (x:xs) = x * (productList xs)

---returns the conjunction of a list
myend::[Bool]->Bool
myend [] = True
myend (x:xs) = x && myend xs

---ﬂattens a list of lists of integers into a single list of integers
concatList::[[Int]]->[Int]
concatList [] = []
concatList (x:xs) = x ++ concatList xs 

---takes a ﬁnite list xs as its argument and returns the list of all the segments of xs
segm::[Integer] -> [[Integer]]
segm [] = []
segm l@(a:as) = [l] ++ segm as 

segment::[[Integer]] -> [[Integer]]
segment [] = []
segment [x] = []
segment l@(a:as) =(segm a) ++ ( segment (init l))

---reverse a list implemented in three different ways
reverseacc::[a]->[a]
reverseacc [] = []
reverseacc (x:xs) = reverseh (x:xs) [] 
	where
		reverseh [] l = []
		reverseh (x:xs) l = l ++ (reverseh xs l) ++ [x]
		


reversefold::[a]->[a]
reversefold l = foldr ( \x-> (\y-> y ++ [x])) [] l

reverse'::[a]->[a]
reverse' [] = []
reverse' (x:xs) = reverse xs ++ [x]

--- prime such that prime i = True if and only if i is a prime number
check::[Bool]->Bool
check [] = True
check (a:as) = if a == True then False else check as

prime::Int -> Bool
prime n = check [ n `mod` x == 0 |x<-[2,3..(n-1)]]	

prime'::Int -> Bool
prime' n = length [n `mod` x == 0 | x<-[2,3..(n-1)]] == 0


---factors n is the list of all the factors of n in increasing order
factor::Int->[Int]
factor 0 = []
factor n = [ x |x<-[1,2..n], n `mod` x == 0]	

---takes a ﬁnite list xs as its argument and returns the list of all the permutations of xs.
perms::[Int] ->[[Int]]
perms [] = [[]]
perms l@(x:xs) = [ [y] ++ l1 | y<-l, l1 <- perms [a|a<-l, a/=y]]


---Greatest Common Divisor
gcd'::Int->Int->Int
gcd' i j 
	| j == 0 = i
	| otherwise = gcd' j (i `mod` j)

gcdlist'::[Int]->[Int]
gcdlist' [] = [0]
gcdlist' l@(x:xs) = [ gcd a x | a <- gcdlist' xs  ]

gcdlist l = t 
	where (t:ts) = gcdlist' l 

---SORTING

---InsertionSort
insert::Integer->[Integer]->[Integer]
insert x [] = [x]
insert x (a:as) = if x<a then x:a:as else a:(insert x as) 

isort::[Integer]->[Integer]
isort [] = []
isort (x:xs) = insert x (isort xs)

---Quicksort
quicksort:: (Ord a) => [a]->[a]
quicksort [] = []
quicksort (x:xs) = 
	smaller ++ [x] ++ bigger 
		where 
		smaller = quicksort [y| y<-xs, y<x]
		bigger = quicksort [y|y<-xs,y>=x]

---MergeSort
merge1::(Ord a)=> [a] -> [a] -> [a]          
merge1 [] [] = []
merge1 [] xs = xs
merge1 xs [] = xs
merge1 (x:xs) (y:ys) = if x>=y then y:(merge1 (x:xs) ys) else x:(merge1 xs (y:ys))

split [] = ([],[])
split (x:y:xs) = ( [x] ++ p, [y] ++ q) where (p,q) = split xs
split xs = (xs,[])

mergesort::(Ord a) => [a]->[a]
mergesort [] = []
mergesort [x] = [x]
mergesort (x:xs) = let (m,n) = split (x:xs)
		in 
		merge1 (mergesort m) ( mergesort n)

---
remove' :: (Eq a) => a-> [a]->[a]
remove' n [] = []
remove' n (x:xs) = if n == x then xs else  x:(remove' n xs)

sort'::(Ord a)=>[a]->[a]
sort' [] = []
sort' l = let 	m = minimum l 
		l' = remove' m l
	in m:(sort' l')

---decides whether a list is sorted
sorted::(Ord a) => [a] -> Bool
sorted [] = True
sorted (x:xs) = ((minimum (x:xs)) == x) && sorted xs


---FOLDS

---the smallest integer in a non-empty list of integers
minlist::[Int]->Int
minlist l = foldr1 (\x -> \y-> min x y) l 

minlistl::[Int]->Int
minlistl l = foldl1 (\x -> \y-> min x y) l 

---takes two strings as its arguments and removes every letter from the second list that occurs in the ﬁrst list
remove::String->String->String
remove m n = foldr (\x -> \y -> if elem x m then y else [x] ++ y ) [] n

---removes adjacent duplicates from a list
remdups::[Int]->[Int]
remdups l = foldr (\x -> \y -> if null y then [x] ++ y else (if x == head y then y else [x] ++ y)) [] l

remdupsl::[Int]->[Int]
remdupsl l = foldl (\x -> \y -> if null x then x ++ [y]  else (if y == last x then x else x ++ [y]  )) [] l

---returns the list of all initial segments of a list
inits::String->[String]
inits m = foldr (\x -> \y -> y ++ [x] ) [] [m] 

---max and min of a list
maximum' :: (Ord a,Num a)=>[a]->a
maximum' l = foldl1 (\acc x -> if acc>x then acc else x )  l        
          
minimum' :: (Ord a,Num a)=>[a]->a  
minimum' l = foldr1 (\x acc -> if  acc<x then acc else x)  l

---map
map':: (a->b)->[a]->[b]          
map' f [] = []         
map' f (x:xs) = foldr (\l acc-> f l : acc) [] (x:xs) 
          
map1 :: (a->b)->[a]->[b]  
map1 f [] = []
map1 f l = foldl (\acc x -> acc ++ [ f x] ) [] l   

---elem 
elem'::(Eq a)=>a->[a]->Bool
elem' n l = foldl (\acc x-> if n==x then True else acc) False l














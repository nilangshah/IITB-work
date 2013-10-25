data Nat = Zero | Succ Nat deriving (Show,Eq)

foldn f id Zero = id
foldn f id (Succ n) = f (foldn f id n)

isZero n = (n==Zero)
mypred n = fst(foldn (\(a,b)->(b,Succ b)) (Zero,Zero) n)

sub n m= foldn f n m
	where f = mypred
add n m= foldn Succ n m


Succ n = foldn Succ (Succ Zero) n 

mult n m = foldn (add n) Zero m
sqr x = mult x x
le n m= isZero(sub n m)
eq n m=isZero(sub n m) && isZero(sub m n)
lt n m=not(isZero (sub m n))

mysqrt n =fst(foldn f (Zero,Zero) n)
	where f (a,b) = if le (sqr (Succ a)) n then (Succ a, sqr (Succ a) ) else (a,b)
	       

mylog n = snd(foldn f ((Succ Zero),Zero) n)
	where f (a,b)= if (le (p a)  n) then ((p a),Succ b) else (a,b) 
	      p a    = mult (Succ(Succ Zero)) a 



import Data.List
euler1 1000 ans=ans
euler1 n ans|n `rem` 3==0=euler1 (n+1) (ans+n)
            |n `rem` 5==0=euler1 (n+1) (ans+n)
            |otherwise=euler1 (n+1) ans   

----------------------------------------------------------------------
fib =0:1:[x+y|(x,y)<-zip fib (tail fib)]
-------------------------------------------------------------------------
euler3 n = maximum (filter (f1) ([x|x<-[1..(m n)],(n `rem` x) ==0]))

f1 x= all (p x) [2..(m x)]
m :: (Integral a)=>a->a
m x= floor (sqrt (fromIntegral x))

p x y=fromIntegral x `rem` y /=0
----------------------------------------------------------
tostr n= [x|x<- f n]
	where f 0 =[]
              f n =(n `rem` 10) : (f (n `div` 10))  

isPali n = m == reverse m
	where m = tostr n

euler4 = maximum( [(x*y)|x<-[-999..0],y<-[-999..0],isPali (x*y)])
------------------------------------------------------------------------------
euler5 =take 1 [x|x<-[1..],check x]
	where check n = all (p n) [1..9]
		where p n x=n `rem` x==0

neuler5 1 = 1
neuler5 n |p==1=n*(neuler5 (n-1))
          |otherwise= (n `div` p)*(neuler5 (n-1))
		where p =gcd n (neuler5 (n-1))

------------------------------------------------------------------------
euler6 n=(p1 n) - (q1 n)
p1 n = ((n*(n+1))/2)^2
q1 1 =1
q1 n=(n^2)+( q1 (n-1))
--------------------------------------------------------------
euler7 n = last(take n [x|x<-[2..],f1 x])

--------------------------------------------------------------------------
no=7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

euler8 =maximum(map (listmul) (segs  p))
	where p=tostr no
              listmul (x:xs)=x*listmul xs
              listmul []=1
segs (x:xs)=([take 5 (x:xs)])++segs (xs)
segs []=[[]]
--------------------------------------------------------------------------------
euler9=[a*b*(sqrt(a^2+b^2))|a<-[1..1000],b<-[1..1000],check1 a b (sqrt(a^2+b^2)) ]
		where check1 a b c =if (a+b+c==1000) then (a^2+b^2==c^2) else False

-----------------------------------------------------------------------------------
euler10 [] ans=ans
euler10 (x:xs) ans |x^2>last (x:xs)=(x:xs)++ans
                   |otherwise= euler10 (nl x (xs)) (x:ans)
                                   where nl x (y:ys)=if (y `rem` x ==0) then nl x ys else y:nl x ys
                                         nl x []=[]

neuler10=sum(euler10 [2..2000000] [])
----------------------------------------------------------------------------------
trian n=(n*(n+1) `div` 2):trian (n+1)

euler12 n i|(countfact n)>499=n
           |otherwise= euler12 (n+i) (i+1)

countfact n =if (floor(sqrt(fromIntegral n))^2==n) then (p-1) else p
                           where p = sum ([2|x<-[1..floor(sqrt(fromIntegral n))],check x]) 
                              		where check x=n `rem`x==0

-----------------------------------------------------------------------

euler14 = maximum ([f x 1|x<-[992763]])
	where	f 1 m = m 
		f x m|x `rem` 2==0 = f (x `div` 2) (m+1)
                     |otherwise = f (3*x+1) (m+1)


-------------------------------------------------------------------------

euler19 m n ans| n<2001=euler19 (p n) (n+1) ((countsun1 m n 1 0):ans)
	       |otherwise=ans 
	where p y=if (leap y) then (((m+367) `mod` 365) `mod` 7) else ((m+366 `mod` 365) `mod` 7)


countsun x y n m|(n==1 || n==3 || n==5||n==7||n==8||n==10)= x:countsun ((x+3) `mod` 7) y (n+1) (l x m)
		|(n==4||n==6||n==9||n==11)=x: countsun ((x+2) `mod` 7) y (n+1) (l x m)
 		|(n==2)=if (leap y) then (x:(countsun (p4 x) y (n+1) (l x m))) else (x:(countsun (p5 x) y (n+1) (l x m)))
		|(n==12)= if (x==0) then [m+1] else [m]
			where p1=(x+3)`mod` 7
			      p2=(x+2) `mod` 7
			      l x m=if(x==0) then (m+1) else m
			      p4 m=((x+1) `mod` 7)
			      p5 m=(x `mod` 7) 
		
			     
leap y 	| ((y`mod` 4 ==0) && (y `mod` 100==0) && (y `mod` 400==0))=True
        |((y`mod` 4 ==0) && (y `mod` 100==0))=False
        |(y`mod` 4 ==0) =True
        |otherwise=False


countsun1 x y n m|(n==1 || n==3 || n==5||n==7||n==8||n==10)= countsun1 ((x+3) `mod` 7) y (n+1) (l x m) 
                |(n==4||n==6||n==9||n==11)=countsun1 ((x+2) `mod` 7) y (n+1) (l x m)
                |(n==2)=if (leap y) then ((countsun1 (p4 x) y (n+1) (l x m))) else ((countsun1 (p5 x) y (n+1) (l x m)))
                |(n==12)= if (x==0) then m+1 else m
                        where p1=(x+3)`mod` 7
                              p2=(x+2) `mod` 7
                              l x m=if(x==0) then (m+1) else m
                              p4 m=((x+1) `mod` 7)
                              p5 m=(x `mod` 7)

-----------------------------------------------------------------------------------------------------------------------------------------
fact n |n>0= n*fact(n-1)
       |otherwise=1

euler20=sum(tostr(fact 100))

-----------------------------------------------------------------

divisibles n m ans|m<ceiling(sqrt(fromIntegral n))=if(n `rem` m==0) then  divisibles n (m+1) (m:(n `div` m):ans) else divisibles n (m+1) ans
		  |otherwise=sum(1:ans)


findsum=[(divisibles x 2 [])|x<-[1..10000]]

euler21a n ans|n<10001=if (n==(divisibles (p n) 2 []) && n/= p n) then euler21a (n+1) (n:ans) else euler21a (n+1) ans
	      |otherwise=ans
		where p a = (divisibles a 2 [])
euler21 =sum(euler21a 1 [])
------------------------------------------------------------------

ablist n|n<28123= if((divisibles n 2 []) > n)then n:ablist(n+1) else ablist(n+1)
	|otherwise=[]

euler23a l=euler23 (ablist 1) (ablist 1) l


euler23 [] [] l=l
euler23 [] (y:ys) l= euler23 (ys) (ys) (nub l) 
euler23 (x:xs) (y:ys) l=if((x+y)<28123) then euler23 xs (y:ys) ((x+y):l) else euler23 [] (y:ys) l

-------------------------------------------------------------------------------
fib25=1:1:[(x+y)|(x,y)<-zip (fib25)(tail fib25)]
euler25 =length(takeWhile p fib25)+1
	where p x =x<10**999

-------------------------------------------------------------------------
euler36=sum([x|x<-[1..1000000],isPali x,y x==reverse(y x)])
	where y x=bin x

bin x |x>0=(x `mod` 2):bin (x `div` 2)
      |otherwise=[]
	 

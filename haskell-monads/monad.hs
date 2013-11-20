import  Debug.Trace

data MayInt a = NothingI | JustI a deriving (Eq,Show)

instance Monad MayInt where
	(>>=) NothingI f  =NothingI
	(>>=) (JustI x) f = f x 
	return = JustI 



data EXP = CON Int | ADD EXP EXP | DIV EXP EXP  

evaL :: EXP -> Maybe Int

evaL (CON i)     = Just i

evaL (ADD e1 e2) = do x1 <- evaL e1
		      x2 <- evaL e2
		      return (x1+x2)

evaL(DIV e1 e2) = do x1 <- evaL e1
		     x2 <- evaL e2
		     if (x2==0) then Nothing else return (x1 `div` x2)

test = evaL(DIV (CON 4) (CON 2))




f :: Int->MayInt Int

f x = if x `mod` 2 == 0  then NothingI else JustI(2*x)

g x = if x `mod` 5 == 0  then NothingI 	else JustI(5*x)

h x = if x `mod` 3 == 0  then NothingI else JustI(3*x)

--mul x =(f x)>>=(\y->g y >>= h)
{--
mul x = do
	 y <- f x
	 z <- g y
	 w <- h z
	 return w  
--}

mul x = (f x)>>=g>>=h






------------------------------------------------------------------------------------------------------------------------
data ListT a = NilL | ConsS a (ListT a) deriving (Eq)
		
instance (Show a) => Show (ListT a) where 
	show t = "["++ init(listShow	t) ++ "]"
		where listShow (NilL)=""
		      listShow (ConsS x xs) = show x ++","++listShow xs        


instance Monad ListT where
	(>>=) x fun  =concatT (mapP (fun) x )
	return x = ConsS x NilL  

concatT :: ListT (ListT a) -> ListT a
concatT (ConsS (ConsS x xs)  xxs) = ConsS x (concatT (ConsS xs xxs))
concatT (ConsS (NilL) xxs) = concatT xxs
concatT NilL = NilL


mapP :: (a -> ListT b)->(ListT a)->(ListT (ListT b))
mapP fun NilL=NilL
mapP fun (ConsS x xs)= ConsS (fun x) (mapP fun xs) 

f1 :: Int -> ListT Int
f1 x = ConsS (x-1) (ConsS x (ConsS (x+1) (NilL)))

fun :: Int -> ListT Int
fun x=ConsS (-x) (ConsS (x) NilL)


listOp x =do  x2<- (f1 x) >>= fun
	      return x2
{--
findSum s = do n1<-[1..6]
	       n2<-[1..6]
	       if ((n1+n2) == s) then return (n1,n2) else []
--}
findSum s = [1..6] >>= (\x1-> [1..6] >>= \x2-> if (x1+x2 ==7 )then return(x1,x2)else [])


----------------------------------------------------------------------------------------------------------------------------------------------




--data Either c b = Left c  | Right b

safeDiv :: Int -> Int -> Either ArithEx Int
safeDiv _ 0 = Left (DivByZero)
safeDiv i j = Right (i `div` j)

data ArithEx = DivByZero | NotDivisible deriving (Eq,Show)

instance Monad (Either e ) where
	(>>=) (Left err) f = Left err
	(>>=) (Right x) f = f x
	return x = Right x


divisionN i j k= do q1<- safeDiv i k 
		    x1 <-return (q1*5) 
		    q2<- safeDiv j k
		    x2<-return (q2*3)
		    return (x1+x2)


{--
divisionN i j k = (safeDiv i k )>>=(\q1-> (safeDiv j k) >>= \q2 -> return (q1+q2))

--}


data Exp =  V Var | PP Var | Add Exp Exp

data Var = A | B | C deriving (Eq,Show)

type State = (Var -> Int , Int)
data StateMonad a = SM ( State -> (a,State) )

instance Monad StateMonad where
	return x = SM (\s-> (x,s) )
	(>>=) (SM sx) k = SM sx'
	  where sx' = \s -> let (i1,s1) = sx s
				SM sx'' = k i1
			     in sx'' s1

eval' :: Exp -> StateMonad Int

eval' (V v) = SM (\(s,c) -> (s v,(s,c)))

eval' (PP v) = SM (\(s,c) -> (s v,(update s v ((s v) + 1),c)))
				where update s v v1 = (\v'-> if (v==v') then v1 else s v')

eval' (Add e1 e2) = do x1 <- eval' e1
		       x2 <- eval' e2
		       x3<-incrementcount 
		       return (x1+x2)

incrementcount = SM f
	where f (s,c) = (5,(s,c+1))

initialstate A=(-4)
initialstate B=(-3)
initialstate C=1
initial = (initialstate,0)


instance Show a => Show (StateMonad a ) where
	show (SM sx)=show(result,statelist ,count)
		where (result,(state,count)) = sx initial
		      statelist      = [(A,state A),(B,state B),(C,state C)] 

z= eval' (Add (Add (PP A)(PP A)) (PP B))



-----------------------------------------------------------------------------------------------------------------------------------------



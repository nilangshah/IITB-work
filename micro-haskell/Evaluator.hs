module Evaluator (eval) where

import Debug.Trace
import AssocList
import Heap
import Utils
import Gcode
import Data.List


eval :: GmState -> [GmState]
eval state = state : restStates
    where restStates | gmFinal state = []
                     | otherwise     = eval nextState
          nextState  = doAdmin (step state)

doAdmin :: GmState -> GmState
doAdmin s = s { gmStats = statIncSteps $ gmStats s }

gmFinal :: GmState -> Bool
gmFinal s | [] <- gmCode s = True
gmFinal _                  = False

step :: GmState -> GmState
step state = dispatch i $ state { gmCode = is }
    where i:is = gmCode state

dispatch :: Instn -> GmState -> GmState
dispatch (PUSHGLOBAL f) = pushglobal f
dispatch (PUSHINT n)    = pushint n
dispatch (PUSHBOOL b)	= pushbool b
dispatch MKAP           = mkap
dispatch (PUSH n)       = push n
dispatch UNWIND         = unwind
dispatch EVAL		= evaL
dispatch ADD		= arith (\x y -> x+y)
dispatch (UPDATE n)	=update n
dispatch (POP n)	=pop n
dispatch MUL		=arith (\x y->x*y)
dispatch SUB		=arith (\x y->x-y)
dispatch CONS		=cons	
dispatch HEAD		=headL	
dispatch RETURN		=returN
dispatch (JUMP s)	=jumP s
dispatch (JFALSE s)  	=jfalsE s
dispatch (EQU)		=comparE (\x y -> x==y)
dispatch (PRINT)        =prinT
dispatch (LABEL s)      = labeL s
dispatch (STOP)		= stoP
dispatch (PUSHNIL)      = pushniL
dispatch (TAIL)         = taiL


taiL :: GmState -> GmState
taiL s = newState.hLookup (gmHeap s) $ a0
        where a0:as=gmStack s
              c    =gmCode s
              newState(NCons n1 n2) = s { gmStack=n2:as }
              newState(NInd ni1)    = s { gmStack=ni1:as, gmCode=[TAIL]++c }

 


pushniL :: GmState -> GmState
{--
pushniL s = s { gmStack = a : gmStack s, gmHeap = heap' }
    where (heap', a) = hAlloc (gmHeap s) $ NNil
--}
pushniL s |elem ba (aDomain g) =  s { gmStack = a : gmStack s}
          |otherwise = s { gmStack = a1:gmStack s,gmHeap = heap',gmGlobals = (ba,a1):g}
                where (heap', a1) = hAlloc (gmHeap s) $ NNil
                      ba          = "nil"
                      g           = gmGlobals s
                      a           = aLookup g ba. error $ "Undeclared global" ++ ba

stoP :: GmState -> GmState
stoP s =s
 

labeL ::String -> GmState -> GmState
labeL st s =s 


jfalsE :: String -> GmState -> GmState
jfalsE label s = newstate . hLookup (gmHeap s) $ a  
	where a:as= gmStack s
	      c    =gmCode s
	      newstate (NBool True)  = s { gmStack=as}
	      newstate (NBool False)  = s {gmStack=as , gmCode=c'}
	      newstate (NInd a1)      = s {gmStack=a1:as,gmCode=[JFALSE label] ++ c   }
	      c'=dropWhile (\x ->x /= (LABEL label)) c


jumP :: String -> GmState -> GmState
jumP label s = s {gmCode=c'}
	where c:c'=dropWhile (\x ->x /= (LABEL label)) (gmCode s) 



prinT :: GmState -> GmState
prinT s = newState . hLookup (gmHeap s) $ a
	where a:as = gmStack s 
	      o    = gmOutput s
	      c    = gmCode s
              newState (NNum n0) = s {gmOutput = o++(show n0) ,gmStack=as}   
	      newState (NBool b0) = s{gmOutput = o++(show b0),gmStack=as  }
	      newState (NInd n0) = s {gmStack=n0:as,gmCode =[PRINT]++c}
	      newState (NNil)    = s {gmOutput = o++(" [])") ,gmStack=as}
              newState (NCons a1 a2) = s {gmOutput=o++" Cons ( ",gmStack = a1:a2:as , gmCode = ([EVAL]++[PRINT]++[EVAL]++[PRINT]++c)} 
{--prinT s = s {gmOutput = o++(show n0), gmStack = as  }
	where a:as        =   gmStack s
              NNum n0     =   hLookup (gmHeap s) $ a
	      o         =   gmOutput s
	      NInd ni  =  

--}

returN ::GmState -> GmState
returN s = s { gmStack = a:s',gmCode=i',gmDump=d}
	where a = last (gmStack s)
	      (i',s'):d=gmDump s


pushglobal :: Name -> GmState -> GmState
pushglobal f s = s { gmStack = a : gmStack s }
    where a = aLookup (gmGlobals s) f . error $ "Undeclared global " ++ f

pushint :: Int -> GmState -> GmState
pushint n s |elem na (aDomain g) =s { gmStack = a : gmStack s}
	    |otherwise = s { gmStack = a1 : gmStack s, gmHeap = heap',gmGlobals = (na,a1):g }
	 	where (heap', a1) = hAlloc (gmHeap s) $ NNum n
	  	      na	  = show n
	              g           = gmGlobals s
                      a           = aLookup g na. error $ "Undeclared global" ++ na

pushbool :: Bool -> GmState -> GmState
pushbool b s |elem ba (aDomain g) =  s { gmStack = a : gmStack s}
	     |otherwise = s { gmStack = a1:gmStack s,gmHeap = heap',gmGlobals = (ba,a1):g}
   	 	where (heap', a1) = hAlloc (gmHeap s) $ NBool b
		      ba          = show b
                      g           = gmGlobals s
                      a           = aLookup g ba. error $ "Undeclared global" ++ ba



mkap :: GmState -> GmState
mkap s = s { gmStack = a:as', gmHeap = heap' }
    where (heap', a) = hAlloc (gmHeap s) $ NAp a1 a2
          a1:a2:as'  = gmStack s

headL :: GmState -> GmState
headL s = newState.hLookup (gmHeap s) $ a0
	where a0:as=gmStack s
	      c    =gmCode s
              newState(NCons n1 n2) = s { gmStack=n1:as } 
	      newState(NInd ni1)    = s { gmStack=ni1:as, gmCode=[HEAD]++c }


cons :: GmState -> GmState
cons s = s { gmStack = a:as', gmHeap = heap' }
    where (heap', a) = hAlloc (gmHeap s) $ NCons a1 a2
          a1:a2:as'  = gmStack s


push :: Int -> GmState -> GmState
push n s = s { gmStack = a:as }
    where as = gmStack s
          a  = as !! (n)
         
slide :: Int -> GmState -> GmState
slide n s = s { gmStack = a : drop n as }
    where a:as = gmStack s

update :: Int -> GmState -> GmState
update n s = s { gmStack = as , gmHeap= heap'}
    where a:as		 = gmStack s
	  ap  	 	 = as !! (n-1)
	  heap' 	 = hUpdate (gmHeap s) (ap) (NInd a)
pop :: Int -> GmState -> GmState
pop n s = s { gmStack = drop n (gmStack s) }

{--
unwind :: GmState -> GmState
unwind s = newState . hLookup (gmHeap s) $ a
    where a:as                   = gmStack s
          newState (NNum n)      = s
          newState (NAp a1 _)    = s { gmCode = [Unwind], gmStack = a1:a:as }
          newState (NGlobal n c) = if length as < n
                                   then error "Unwinding too few arguments"
                                   else s { gmCode = c }  --}
unwind :: GmState -> GmState
unwind s= newState . hLookup (gmHeap s) $ a
	where a:as		=gmStack s
	      (i',s'):d		=gmDump s
	      newState(NNum n)	=s { gmCode=i' , gmStack = a:s' ,gmDump=d  }
	      newState(NNil)    =s {gmCode=i' , gmStack =a:s' , gmDump=d  }
	      newState(NCons n1 n2)	=s { gmCode=i' , gmStack = a:s' ,gmDump=d  }
	      newState (NAp a1 _)    = s { gmCode = [UNWIND], gmStack = a1:a:as }
	      newState (NInd b1)      = s {gmCode=[UNWIND],gmStack=b1:as }
              newState (NGlobal n c) = if length as < n
                                   then error "Unwinding too few arguments"
                                   else s { gmCode = c,gmStack =rearrange n (gmHeap s) (gmStack s)  }


rearrange :: Int -> GmHeap -> GmStack -> GmStack
rearrange n heap as= take n as' ++ drop n as
		where as' = map (getArg . hLookup heap) (tail(as))
		      getArg (NAp _ a) = a





arith :: (Int->Int->Int)->GmState -> GmState
{--arith f s= s { gmStack=a:as , gmHeap=heap'}
	where a0:a1:as = gmStack s
	      NNum n0      =hLookup (gmHeap s) $ a0
	      NNum n1      =hLookup (gmHeap s) $ a1	 
	      (heap' ,a)=hAlloc (gmHeap s) $ NNum (f n0 n1)

--}

arith = primitive2 boxInteger unboxInteger

{--
comparE :: (Int -> Int -> Bool) -> GmState -> GmState
comparE = primitive2 boxBoolean unboxInteger
--}


comparE :: (Int->Int->Bool)->GmState -> GmState
comparE f s= newState  (hLookup (h) $ a0) (hLookup (h) $ a1)
	where a0:a1:as = gmStack s
	      l        = length(gmStack s) 
	      h        = gmHeap s
	      c        = gmCode s
	      newState (NBool b1) (NBool b2 ) = if (l `mod`2==0) then (if b1 then s{gmStack=a1:as,gmCode=[EQU]++c} else s{gmStack=h2:(drop 1 as),gmHeap=heap2'}) else (if b1==b2 then s{gmStack=h3:as,gmHeap=heap3'} else s{gmStack=h2:as,gmHeap=heap2'})
	      newState (NBool b1) (a) = if (l `mod`2==0) then (if b1 then s{gmStack=a1:as,gmCode=[EQU]++c} else s{gmStack=h2:(drop 1 as),gmHeap=heap2'}) else (s{gmStack=a1:a0:as,gmCode = [EVAL,EQU]++c}) 
	      newState (NAp ap1 ap2) (_)=s { gmCode=[EVAL,EQU]++c}
	      newState (_) (NAp ap1 ap2)=s {gmStack = a1:a0:as,gmCode=[EVAL,EQU]++c }
	      newState (NCons nc1 nc2) (NCons nc3 nc4) = (s{gmStack = nc1:nc3:nc2:nc4:as,gmCode=[EQU,EQU]++c})
	      newState (NInd ni1) (NInd ni2) = if ((ni1)==(ni2) ) then (s{gmStack = h3:as,gmHeap=heap3'})else (s{gmStack=ni1:ni2:as,gmCode=[EQU]++c})
	      newState (NInd ni1) (_) = (s{gmStack = ni1:a1:as,gmCode=[EVAL,EQU]++c}) 
	      newState (_) (NInd ni1) = (s{gmStack = ni1:a0:as,gmCode=[EVAL,EQU]++c})
	      newState (ni1) (ni2) = s { gmStack = h1:as, gmHeap=heap'}
                        where (heap',h1)=hAlloc (gmHeap s) $ NBool ((getval ni1 s)==(getval ni2 s))
              (heap2',h2)=hAlloc (gmHeap s) $ NBool (False)
	      (heap3',h3)=hAlloc (gmHeap s)$ NBool(True)  

getValA a s =  hLookup (gmHeap s) $ a

getval (NInd n1) s =newState (hLookup (gmHeap s) $ n1)
			where newState (NInd n2) = getval (NInd n2) s 
			      newState (a1)       = a1
getval (n1) s = n1
 
evaL :: GmState -> GmState
evaL s= newstate . hLookup (gmHeap s) $ a
	where a:as		   = gmStack s
	      d			   = gmDump s
	      c			   = gmCode s
	      newstate(NAp a1 a2)  = s { gmStack=[a],gmCode=[UNWIND],gmDump=(c,as):d}  
	      newstate(NGlobal 0 c1)= s { gmStack=[a],gmCode=c1,gmDump=(c,as):d }
	      newstate(NGlobal n c1) = s {gmStack=[a],gmCode=[UNWIND],gmDump=(c,as):d}
	      newstate(NNum n) =  s 
	      newstate(NInd n) = s {gmStack=n:as,gmCode=[EVAL]++c  }
	      newstate(NCons a1 a2) = s
	      newstate(NNil) =s
	      newstate(NBool b)=s

{--
s {gmCode = [UNWIND], gmStack=[a] , gmDump=(i,as):d }
	where (a:as) =  gmStack s
	      i	     =  gmCode s
	      d      =  gmDump s
		--}
boxInteger :: Int -> GmState -> GmState
boxInteger n s =s { gmStack = a : gmStack s , gmHeap= h' }
			where (h', a) = hAlloc (gmHeap s) (NNum n)

unboxInteger :: Addr -> GmState -> Int
unboxInteger a s= ub (hLookup (gmHeap s) $ a)
			where ub (NNum i) = i
			      ub (NInd a1)= unboxInteger a1 s




primitive1 :: (b -> GmState -> GmState)
	      -> (Addr -> GmState -> a)
	      -> (a -> b)
	      -> (GmState -> GmState)




primitive1 box unbox op s= box (op (unbox a s)) (s {gmStack= as})
				where (a:as) = gmStack s



primitive2 :: (b -> GmState -> GmState)
	-> (Addr -> GmState -> a)
	-> (a -> a -> b)
	-> (GmState -> GmState)

primitive2 box unbox op s = box (op (unbox a0 s) (unbox a1 s)) (s{gmStack=as})
				where (a0:a1:as) = gmStack s



arithmetic1 ::(Int -> Int)	-- arithmetic operator
		-> (GmState -> GmState) -- state transition

arithmetic1 = primitive1 boxInteger unboxInteger
arithmetic2 ::(Int -> Int -> Int)	-- arithmetic operation
	      -> (GmState -> GmState) -- state transition

arithmetic2 = primitive2 boxInteger unboxInteger


boxBoolean :: Bool -> GmState -> GmState
boxBoolean b s=s { gmStack =(a: gmStack s) ,gmHeap =h'}
		where(h',a) = hAlloc (gmHeap s) (NNum b')
		     b'	|b         = 1
		        |otherwise = 0

































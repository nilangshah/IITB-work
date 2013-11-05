import Data.List
type Fname = String
type Var = String

data Program = Prog [Fundef] Exp deriving (Show,Eq)
data Fundef = Fun String [String] Exp deriving (Show,Eq)
data Exp = I Int | B Bool | V String | Nil | Fname String | App Exp Exp
           deriving (Show,Eq)                            

--Fun String [String] Exp
type Code = [Instn]
gencpgm :: Program -> Code
count=0

gencpgm (Prog (x:xs) y)=fun (x:xs)++[LABEL "MAIN"] ++ exp1 y count [] ++[EVAL]++[PRINT]++[STOP]++builtin
gencpgm (Prog [] y)=[LABEL "MAIN"] ++ exp1 y count [] ++[EVAL]++[PRINT]++[STOP]++builtin

fun []= [] 
fun (x:xs)= abc x++ [UNWIND] ++ fun xs
 
abc (Fun x (y) z) = [GLOBSTART x p] ++ (exp1 z (count) y) ++ [UPDATE (p+1)] ++ [POP (p)]
		where   p = length (y)

exp1 (App x y) a y1=exp1 y (a) y1++ exp1 x (a+1) y1++ [MKAP] 
exp1 (I x) a y=  [PUSHINT x] 
exp1 (B x) a y=  [PUSHBOOL x]
exp1 (Nil) a y= [PUSHNIL]
{--exp1 (Fname "cons") a y = [PUSHGLOBAL "cons"]
exp1 (Fname "+") a y = [PUSHGLOBAL "+"]
exp1 (Fname "-") a y = [PUSHGLOBAL "-"]
exp1 (Fname "==") a y = [PUSHGLOBAL "=="]
exp1 (Fname "if") a y = [PUSHGLOBAL "if"]
exp1 (Fname "*") a y = [PUSHGLOBAL "*"]
--}
exp1 (Fname x) a y=  [PUSHGLOBAL x]

exp1 (V x) a y=  [PUSH (p1 a x)]
	where p1 a x = a + (head (elemIndices  x (y)))
	      p = length (y)		

builtin =carfun++cdrfun++nullfun++eqfun++iffun++addfun++subfun++mulfun++confun++headfun++tailfun

addfun=[GLOBSTART "+" 2]++[PUSH 1]++[EVAL]++[PUSH 1]++[EVAL]++[ADD]++[UPDATE 3]++[POP 2]++[RETURN]
subfun= [GLOBSTART "-" 2]++[PUSH 1]++[EVAL]++[PUSH 1]++[EVAL]++[SUB]++[UPDATE 3]++[POP 2]++[RETURN]
confun= [GLOBSTART "cons" 2]++[CONS]++[UPDATE 1]++[RETURN]
headfun=[GLOBSTART "head" 1]++[EVAL]++[HEAD]++[EVAL]++[UPDATE 1]++[UNWIND]
iffun=[GLOBSTART "if" 1]++[PUSH 0]++[EVAL]++[JFALSE "L1"]++[PUSH 1]++[JUMP "L2"]++[LABEL "L1"]++[PUSH 2]++[LABEL "L2"] ++[EVAL]++[UPDATE 4]++[POP 3]++[UNWIND]
tailfun=[GLOBSTART "tail" 1]++[EVAL]++[TAIL]++[EVAL]++[UPDATE 1]++[UNWIND]
mulfun=[GLOBSTART "*" 2]++[PUSH 1]++[EVAL]++[PUSH 1]++[EVAL]++[MUL]++[UPDATE 3]++[POP 2]++[RETURN]
eqfun=[GLOBSTART "==" 2]++[PUSH 1]++[EVAL]++[PUSH 1]++[EVAL]++[EQU]++[UPDATE 3]++[POP 2]++[RETURN]
carfun=[GLOBSTART "car" 1]++[EVAL]++[HEAD]++[EVAL]++[UPDATE 1]++[UNWIND]
cdrfun=[GLOBSTART "cdr" 1]++[EVAL]++[HEAD]++[EVAL]++[UPDATE 1]++[UNWIND]
nullfun=[GLOBSTART "null" 1]++[PUSHNIL]++[PUSH 1]++[PUSHGLOBAL "=="]++[MKAP]++[MKAP]++[UPDATE 2]++[POP 1]++[UNWIND]



{--
type Fname = String
type Var = String
data Program = Prog [Fundef] Exp deriving (Show,Eq)
data Fundef = Fun String [String] Exp deriving (Show,Eq)
data Exp = I Int | B Bool | V String | Nil | Fname String | App Exp Exp
--}
 


data Instn = PUSH Int | PUSHINT Int | PUSHGLOBAL String |
		PUSHBOOL Bool | PUSHNIL | POP Int |
		EVAL | UNWIND | MKAP | UPDATE Int | RETURN |
		LABEL String | JUMP String | JFALSE String |
		ADD | SUB | MUL | CONS | HEAD | TAIL | IF | EQU |
		GLOBSTART String Int | PRINT | STOP

instance Show Instn where show (PUSH i) = "PUSH " ++ show i ++ "\n"
			  show (PUSHINT i) = "PUSHINT " ++ show i ++ "\n"
			  show (PUSHGLOBAL str) = "PUSHGLOBAL " ++ show str ++ "\n"
		          show (PUSHBOOL b) = "PUSHBOOL " ++ show b ++ "\n"
			  show PUSHNIL = "PUSHNIL " ++ "\n"
			  show (POP i) = "POP " ++ show i ++ "\n"
			  show EVAL = "EVAL" ++ "\n"
		          show UNWIND = "UNWIND" ++ "\n"
			  show MKAP = "MKAP" ++ "\n"
		          show RETURN = "RETURN" ++ "\n"
			  show (UPDATE i) = "UPDATE " ++ show i ++ "\n"
			  show (LABEL str) = "LABEL " ++ show str ++ "\n"
			  show (JUMP str) = "JUMP " ++ show str ++ "\n"
			  show (JFALSE str) = "JFALSE " ++ show str ++ "\n"
			  show ADD = "ADD" ++ "\n"
			  show SUB = "SUB" ++ "\n"
			  show MUL = "MUL" ++ "\n"
			  show CONS = "CONS" ++ "\n"
			  show HEAD = "HEAD" ++ "\n"
			  show TAIL = "TAIL" ++ "\n"
			  show IF = "IF" ++ "\n"
			  show EQU = "EQU" ++ "\n"
			  show (GLOBSTART str i) = "\n GLOBSTART " ++ show str ++ " " ++show i ++ "\n"
			  show PRINT = "PRINT" ++ "\n"
			  show STOP = "STOP" ++ "\n"





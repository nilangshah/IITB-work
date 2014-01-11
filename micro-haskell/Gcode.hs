module Gcode where

import Parser
{--
type Fname = String
type Var = String

data Program = Prog [Fundef] Exp deriving (Show,Eq)
data Fundef = Fun String [String] Exp deriving (Show,Eq)
data Exp = I Int | B Bool | V String | Nil | Fname String | App Exp Exp
           deriving (Show,Eq)                            


--}
type Code = [(String , Int , [Instn])]
gencpgm :: Program -> Code
data Instn = PUSH Int | PUSHINT Int | PUSHGLOBAL String |
		PUSHBOOL Bool | PUSHNIL | POP Int |
		EVAL | UNWIND | MKAP | UPDATE Int | RETURN |
		LABEL String | JUMP String | JFALSE String |
		ADD | SUB | MUL | CONS | HEAD | TAIL | IF | EQU |
		GLOBSTART String Int | PRINT | STOP deriving Eq
instance Show Instn where
	show (PUSH i) = " PUSH " ++ show i ++ "\n"
	show (PUSHINT i) = " PUSHINT " ++ show i ++ "\n"
	show (PUSHGLOBAL str) = " PUSHGLOBAL " ++ show str ++ "\n"
	show (PUSHBOOL b) = " PUSHBOOL " ++ show b ++ "\n"
	show PUSHNIL = " PUSHNIL " ++ "\n"
	show (POP i) = " POP " ++ show i ++ "\n"
	show EVAL = " EVAL" ++ "\n"
	show UNWIND = " UNWIND" ++ "\n"
	show MKAP = " MKAP" ++ "\n"
	show RETURN = " RETURN" ++ "\n"
	show (UPDATE i) = " UPDATE " ++ show i ++ "\n"
	show (LABEL str) = "LABEL " ++ show str ++ "\n"
	show (JUMP str) = " JUMP " ++ show str ++ "\n"
	show (JFALSE str) = " JFALSE " ++ show str ++ "\n"
	show ADD = " ADD" ++ "\n"
	show SUB = " SUB" ++ "\n"
	show MUL = " MUL" ++ "\n"
	show CONS = " CONS" ++ "\n"
	show HEAD = " HEAD" ++ "\n"
	show TAIL = " TAIL" ++ "\n"
	show IF = " IF" ++ "\n"
	show EQU = " EQU" ++ "\n"
	show (GLOBSTART str i) = "\n GLOBSTART " ++ show str ++ " " ++ show i ++ "\n"
	show PRINT = " PRINT" ++ "\n"
	show STOP = " STOP" ++ "\n"

builtin "+" = [  ("+", 2,[PUSH 1,EVAL,PUSH 1,EVAL,ADD,UPDATE 3,POP 2,RETURN ])]
                                    
builtin "-" = [ ("-", 2,[PUSH 1,EVAL,PUSH 1,EVAL,SUB,UPDATE 3,POP 2,RETURN])]
                                  
builtin "*" = [ ("*", 2 ,
                [PUSH 1,
                EVAL,
                PUSH 1,
                EVAL,
                MUL,
                UPDATE 3,
                POP 2,
                RETURN ])]

builtin "==" = [ ("==", 2,
                [PUSH 1,
                EVAL,
                PUSH 1,
                EVAL,
                EQU,
                UPDATE 3,
                POP 2,
                RETURN ])]
                                  
builtin "if" = [ ("if", 3,
                 [PUSH 0,
                 EVAL,
                 JFALSE "L1",
                 PUSH 1,
                 JUMP "L2",
                 LABEL "L1",
                 PUSH 2,
                 LABEL "L2",
                 EVAL,
                 UPDATE 4,
                 POP 3,
                 UNWIND ])]
               
builtin "cons" = [ ("cons",2,
                   [CONS,
                   UPDATE 1,
                   RETURN ])]
                 
builtin "car" = [ ( "car", 1,
                   [EVAL,
                   HEAD,
                   EVAL,
                   UPDATE 1,
                   UNWIND ])]
                 
builtin "cdr" = [ ("cdr",1,
                   [EVAL,
                   TAIL,
                   EVAL,
                   UPDATE 1,
                   UNWIND ])]

builtin "null" = [ ("null",1, 
			[EVAL,
			PUSHNIL,			
			EQU, 
			UPDATE 1, 
			RETURN])]
                                            

ff (Fun name args expr) = [(name,d,gcode_expr)]
                             where gcode_expr = r expr rho d
                                   rho = zip args (reverse [1..d])
                                   d = length args
                                   
r expr rho d = (c expr rho d) ++ [UPDATE (d+1),POP d,UNWIND]

get_var_index var [] = 0
get_var_index var ((x,n):xs) | var==x = n
                             | otherwise=get_var_index var xs

  
  
  
c (I i) _ _ = [PUSHINT i]
c (B b) _ _ = [PUSHBOOL b]
c (V v) rho d = [PUSH (d-offset)] 
                where offset = get_var_index v rho
c (Fname f) _ _= [PUSHGLOBAL f]
c (Nil) _ _ = [PUSHNIL]
c (App e1 e2) rho d = (c e2 rho d )++(c e1 rho (d+1))++[MKAP]

gencpgm (Prog fdefs main) = (concat (map ff fdefs))++f_main ++ builtin_defs
                            where f_main = [ ("main",0,
                                            ((c main [] 0)++
                                            [EVAL,
                                            PRINT,
                                            STOP]))]
                                            
builtin_defs = concat ( map builtin ["+","-","*","==","if","cons","car","cdr","null"])


mapprogram = Prog 
             [Fun "map" ["f","l"] (App (App (App (Fname "if") 
                                             (App (Fname "null") (V "l"))) 
                                        Nil) 
                                   (App (App (Fname "cons") (App (V "f") (App (Fname "car") (V "l")))) 
                                    (App (App (Fname "map") (V "f")) 
                                     (App (Fname "cdr") (V "l"))))),
              Fun "square" ["a"] (App (App (Fname "*") (V "a")) (V "a"))] 
             (App (App (Fname "map") (Fname "square")) 
              (App (App (Fname "cons") (I 1)) (App (App (Fname "cons") (I 2)) (App (App (Fname "cons") (I 3)) Nil))))




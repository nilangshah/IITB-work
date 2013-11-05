import Data.Char


{--
Some elementary parser 
--}
type Parser symbol result  =  [symbol] -> [([symbol],result)]

{- A parser that yields just one solution and no rest-string
   is called a deterministic parser:
-}

type DetPars symbol result  =  [symbol] -> result

------------------------------------------------------------
-- Priorities of operators

infixr 6  <*> , <*   ,  *> , <:*>
infixl 5  <@  , <?@
infixr 4  <|>


------------------------------------------------------------
-- Auxiliary functions

list (x,xs)  =  x:xs
single x     =  [x]

ap2 (op,y)   = (`op` y)
ap1 (x,op)   = (x `op`)

ap  (f,x)    = f x
ap' (x,f)    = f x


------------------------------------------------------------
-- Trivial parsers

fail    ::  Parser s r
fail xs  =  []

succeed       ::  r -> Parser s r
succeed v xs   =  [ (xs,v) ]

epsilon  ::  Parser s ()
epsilon   =  succeed ()

------------------------------------------------------------
-- Elementary parsers

satisfy  ::  (s->Bool) -> Parser s s
satisfy p []     =  []
satisfy p (x:xs) =  [ (xs,x) | p x ]

symbol  ::  Eq s  =>  s -> Parser s s
symbol   =  satisfy . (==)

symbol' a []      =  []
symbol' a (x:xs)  =  [ (xs,x) | a==x ]

symbol'' a []                    =  []
symbol'' a (x:xs)  |  a==x       =  [ (xs,x) ]
                   |  otherwise  =  []


token  ::  Eq s  =>  [s] -> Parser s [s]
token   =  Main.sequence . map symbol 


token' k xs  |  k==take n xs  =  [ (drop n xs, k) ]
             |  otherwise     =  []
                          where  n = length k

------------------------------------------------------------
-- Parser combinators 

(<*>)          ::  Parser s a -> Parser s b -> Parser s (a,b)
(p1 <*> p2) xs  =  [  (xs2,(v1,v2)) 
                   |  (xs1, v1) <- p1 xs
                   ,  (xs2, v2) <- p2 xs1
                   ]

(<|>)          ::  Parser s a -> Parser s a -> Parser s a
(p1 <|> p2) xs  =  p1 xs ++ p2 xs

(<@)          ::  Parser s a -> (a->b) -> Parser s b
(p <@ f) xs   =   [ (ys, f v)
                  | (ys,   v) <- p xs
                  ]

option   ::  Parser s a -> Parser s [a]
option p  =  p  <@  single
             <|> succeed []

many     ::  Parser s a  -> Parser s [a]
many p    =  p <:*> (many p)  <|>  succeed []

many1    ::  Parser s a -> Parser s [a]
many1 p   =  p <:*> many p 


-----------------------------------------------------------
-- Determinsm


determ :: Parser a b -> Parser a b
determ p xs  |  null r     =  []
             |  otherwise  =  [head r]
                     where r = p xs

compulsion = determ . option

greedy = determ . many

greedy1 = determ . many1


------------------------------------------------------------
-- Abbreviations

(<*)         ::  Parser s a -> Parser s b -> Parser s a
p <* q        =  p <*> q  <@  fst

(*>)         ::  Parser s a -> Parser s b -> Parser s b
p *> q        =  p <*> q  <@  snd

(<:*>)       ::  Parser s a -> Parser s [a] -> Parser s [a]
p <:*> q      =  p <*> q  <@  list

(<?@)        ::  Parser s [a] -> (b,a->b) -> Parser s b
p <?@ (n,y)   =  p <@ f
          where  f []  = n
                 f [h] = y h


pack         ::  Parser s a -> Parser s b -> Parser s c -> Parser s b
pack s1 p s2  =  s1 *> p <* s2

listOf       ::  Parser s a -> Parser s b -> Parser s [a]
listOf p s    =  p <:*> many (s *> p)  <|>  succeed []

chainl       ::  Parser s a -> Parser s (a->a->a) -> Parser s a
chainl p s    =  p <*> many (s <*> p)  <@  uncurry (foldl (flip ap2))


chainr       ::  Parser s a -> Parser s (a->a->a) -> Parser s a
chainr p s    =  many (p <*> s) <*> p  <@  uncurry (flip (foldr ap1))

sequence     ::  [Parser s a] -> Parser s [a]
sequence      =  foldr (<:*>) (succeed [])

choice       ::  [Parser s a] -> Parser s a
choice        =  foldr (<|>) Main.fail

chainl' p s   =  q
          where  q = (option (q <*> s) <?@ (id,ap1) ) <*> p <@ ap

chainr' p s   =  q
          where  q = p <*> (option (s <*> q) <?@ (id,ap2) ) <@ ap'

------------------------------------------------------------
-- Parser transformators


just      ::  Parser s a -> Parser s a
just p  =  filter (null.fst) . p

ign :: String->Bool
ign ys= (null ys) || check (ys) False

just'      ::  Parser Char a -> Parser Char a
just' p xs  =  [ ([],v) | (ys,v) <- p xs , ign ys  ]
                          
some   ::  Parser s a -> DetPars s a
some p  =  snd . head . just p
some' p  =  snd . head . just' p

check (x:xs) m |  x ==' ' = check xs True
               | otherwise= check [] False 
check [] m = m           

------------------------------------------------------------
-- Some common special cases


identifier  ::  Parser Char String
identifier   =  satisfy isAlpha <:*> greedy (satisfy isAlphaNum)

digit       ::  Parser Char Int
digit        =  satisfy isDigit  <@  f
         where  f c = ord c - ord '0'


natural     ::  Parser Char Int
natural      =  greedy1 digit  <@  foldl f 0
         where  f a b = a*10 + b

integer     ::  Parser Char Int
integer      =  option (symbol '-') <*> natural  <@  f
         where  f ([],n) =  n
                f (_ ,n) =  -n

integer'    ::  Parser Char Int
integer'     =  (option (symbol '-') <?@ (id,const negate)) <*> natural  <@ ap

fixed       ::  Parser Char Float
fixed        =  (integer <@ fromInteger.toInteger)
                <*> 
                (option (symbol '.' *> fractpart)  <?@  (0.0,id))
                <@  uncurry (+)

fractpart   ::  Parser Char Float
fractpart    =  greedy digit  <@  foldr f 0.0
         where  f d n = (n + fromInteger (toInteger d))/10.0


float       ::  Parser Char Float
float        =  fixed 
                <*> 
                (option (symbol 'E' *> integer) <?@ (0,id) )
                <@ f
         where  f (m,e)  =  m * power e
                power e | e<0       = 1.0 / power (-e)
                        | otherwise = fromInteger(10^e)


sp  ::  Parser Char a -> Parser Char a
sp   =  (greedy (satisfy isSpace) *> )

sp'  =  ( . (dropWhile isSpace))


sptoken  ::  String -> Parser Char String
sptoken   =  sp . token

spsymbol ::  Char -> Parser Char Char
spsymbol  =  sp . symbol

spident  ::  Parser Char String
spident   =  sp identifier


parenthesized, bracketed,
 braced, angled, quoted   :: Parser Char a -> Parser Char a
parenthesized p = pack (spsymbol '(')  p (spsymbol ')')
bracketed p     = pack (spsymbol '[')  p (spsymbol ']')
braced p        = pack (spsymbol '{')  p (spsymbol '}')
angled p        = pack (spsymbol '<')  p (spsymbol '>')
quoted p        = pack (spsymbol '"')  p (spsymbol '"')
stropped p      = pack (spsymbol '\'') p (spsymbol '\'')

commaList, semicList  ::  Parser Char a -> Parser Char [a]
commaList p  =  listOf p (spsymbol ',')
semicList p  =  listOf p (spsymbol ';')

twopass ::  Parser a b -> Parser b c -> Parser a c
twopass lex synt xs = [ (rest,tree) | (rest,tokens) <- many lex xs , (_,tree)      <- just synt tokens ]

{--
This is the grammar of μ-Haskell:


program → {fundef }∗ exp

fundef→ fname {var}∗ = exp
exp → con | var | exp + exp | exp − exp | fname {exp}∗ |
		exp == exp | if exp then exp else exp | exp : exp | car exp | cdr exp | null exp | [exp {, exp}∗ ] | (exp)

con → intlit | boollit| nil

{x}∗ above stands for 0 or more repetitions of x. Thus [exp {, exp}∗ ] stands for lists of length ≥ 1.
nil represents empty list.

--}

{--
Data structures to be used for creating parse tree..
--}
{--
type Fname = String
type Var = String
data Program = Prog [Fundef] Exp deriving Show
data Fundef = Fun String [String] Exp deriving Show
data Exp = I Int | V Var | B Bool | Nil | Fname String | App Exp Exp deriving Show
--}

{--
An environment is list of pairs in which a finite mapping can be represented.
function assoc can be used to associate a value to its image in an environment.
--}

-------------------------------------------------------------------------

--Major Assignment --
type Fname = String
type Var = String
data Program = Prog [Fundef] Exp deriving Show
data Fundef = Fun String [String] Exp deriving Show
data Exp= I Int | V Var | B Bool | Nil  | Null Exp | Fname String| App Exp Exp deriving Show


-- chainx :: Parser s a -> Parser s (a->a->a) -> Parser s a
-- chainx p s = (many (p <*> s) <*> p) <@ uncurry(flip(foldr ap1 ))
-- where ap1 (x, op) y |y==[]= x `op` NilList
-- |otherwise = x `op` y
					   
parseit = some'((( many (funcdef <* ( (symbol '\n')))) <*>sp expr) <*(many(symbol '\n')))


myshow (x:xs,y)  =  ("\n" ++ show(x)++"\n"++ myshow(xs,y) ) 
myshow ([],y)=("\n"++show(y)++"\n")
main = do
	input <- readFile "pfile"
        print input
        let	
	 output = (parseit input) 
        putStrLn $ (myshow output )   
       
-- int parser
inte :: Parser Char Exp
inte = sp (natural) <@ I

boolP :: Parser Char String
boolP=token' "True" <|> token' "False"

--bool parser
bool :: Parser Char Exp
bool = (sp boolP <@ (read::String -> Bool)) <@ B

isnil :: Parser Char Exp
isnil =  (token' "") <@ \x-> Nil

keyWords = ["if" ,"car","cdr","null", "then","else"]
identifier1 p           | id2 p == [] = []
  			| not(snd (head (id2 p)) `elem` keyWords) = id2 p
                	| otherwise = []
id2 = satisfy isAlpha <:*> id1
id1 s= [(dropWhile isAlphaNum s,takeWhile isAlphaNum s)]


isVar :: Parser Char Exp
isVar=sp identifier1 <@ V

 
expr::Parser Char Exp
expr= ifloop 

equals=chainl (sp cons) ((sp (token' "==")) <@ \x-> h ) <|> cons
	 where  h x y=(App (App (Fname "==") x) y)  
   

fname =( (((sp identifier ) <@ Fname ) <*> (many ( (  (token " "))<*>(sp fact) ))) <@ uncurry (foldl gp) )  

  				
gp x (op,y) = App x y

funcdef :: Parser Char Fundef
funcdef= ((((sp identifier) <* (( symbol  ' ') )) <*> listOf (sp identifier) ( symbol ' ' ))  <*> (spsymbol '=') *> sp expr )  <@ \((x,y),z) -> Fun x y z
  

cons =((sp expr1 <*>(many ((symbol ':')*> sp expr1) ) )<@ \(x,ys) ->(foldr1 fp (x:ys))) <|> expr1 
fp x y = App (App (Fname "Cons") x) y      
expr1=chainl (sp term) (((sp (symbol '+')) <@ \x-> f)  <|> ((sp (symbol '-')) <@ \x-> g )) <|> term 
         where  g x y=(App (App (Fname "-") x) y)  
	        f x y=(App (App (Fname "+") x) y)
             
             

-- cons=( sp term <*> (symbol ':' ) <*> sp term) <@ \(x,(y,z))->App (App (Fname "cons") x) z
       
fact:: Parser Char Exp 
fact=inte <|> parenthesized expr <|> isVar <|> listc_blank <|> listc <|> bool <|> cdrP <|> carP <|> nullP <|>isnil 

term= fact   <|> fname 
listc_blank =  (sp(symbol '[') <*> sp(symbol ']') <@ \(x,y) -> Nil)
listc = 	(sp (symbol '[') *> chainv (sp expr) (sp (symbol ',')) <* sp (symbol ']'))

chainv p s = listOf p s <@ foldr app Nil
  where app x y=(App (App (Fname "cons") x) y)
       
 
ifloop= (((( token' "if") <@ Fname) <*> sp equals <*>(sp (token' "then") )*>sp equals <*>(sp (token' "else") )*>sp equals )
 <@ \(x,(y,(w,z))) -> App ( App ( App x y ) w ) z ) <|> (sp equals)




cdrP = (((token' "cdr") ) <*> ( sp fact )  ) <@ \(_,x)->App (Fname "cdr") x
carP = (((token' "car")  ) <*> ( sp fact ) ) <@ \(_,x)->App (Fname "Car") x
nullP = (((token' "null") ) <*> ( sp fact) )  <@ \(_,x)->App (Fname "Null") x
















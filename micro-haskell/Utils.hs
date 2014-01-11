module Utils where

import Data.List (intercalate)

import AssocList
import Heap
import Debug.Trace
import Gcode
data GmState = GmState {gmOutput   ::  GmOutput 
		       ,gmCode    :: GmCode    -- Current instruction stream
                       , gmStack   :: GmStack   -- Current stack
		       , gmDump    :: GmDump	   
                       , gmHeap    :: GmHeap    -- Heap of nodes
                       , gmGlobals :: GmGlobals -- Global addresses in heap
                       , gmStats   :: GmStats   -- Statistics
                       }
type GmOutput=[Char]
type GmCode = [Instn]
type GmStack = [Addr]
type GmHeap = Heap Node
type GmGlobals = ASSOC Name Addr
type GmDump = [GmDumpItem]
type GmDumpItem = (GmCode, GmStack)
{--
data Instruction
    = Unwind
    | Pushglobal Name
    | Pushint Int
    | Push Int
    | Mkap
    | Update Int
    | Pop Int
    | Eval
    | Add
    | Sub
    | Mul 
    | Div 
    | Neg
    | Eq | Ne | Lt | Le | Gt | Ge
    deriving (Eq, Show)
--}
data Node
    = NNum Int
    | NBool Bool
    | NNil 
    | NAp Addr Addr
    | NGlobal Int GmCode
    | NCons Addr Addr
    | NInd Addr
  deriving (Show)

instance Eq Node
	where NNum a == NNum b = a==b
	      NAp a b == NAp c d = (a==c) && (b==d)
	      NGlobal a b == NGlobal c d = False
	      NInd a ==NInd b = a==b
	      NBool a == NBool b = a==b
	      NNil == NNil = True 
	      NNil == _    = False
	      _    == NNil = False 
	      (NCons a1 a2) == (NCons b1 b2) = (trace $ show [a1,a2,b1,b2])((a1==b1) && (a2==b2))
	      a    == b      =trace(show a++show b) False 
type Name = String

type GmStats = Int
statInitial :: GmStats
statInitial = 0
statIncSteps :: GmStats -> GmStats
statIncSteps s = s + 1
statGetSteps :: GmStats -> Int
statGetSteps s = s

showResults :: [GmState] -> String
showResults states@(s:_) = scs ++ "\n\n" ++ "State transitions\n\n" ++ sts ++ "\n\n" ++
                           showStats (last states)
    where scs = intercalate "\n" $ map (showSC s) (gmGlobals s)
          sts = intercalate "\n" $ map showState states

showSC :: GmState -> (Name, Addr) -> String
showSC s (name, addr) = "Code for " ++ name ++ ":\n" ++ showInstructions code
    where (NGlobal arity code) = hLookup (gmHeap s) addr

showInstructions :: GmCode -> String
showInstructions is = "  " ++ intercalate "\n  " (map show is) ++ "\n"

showState :: GmState -> String
showState s = showStack s ++ "\n" ++ showInstructions (gmCode s) ++ "\n"

showStack :: GmState -> String
showStack s = "Stack:[" ++ intercalate "\n       " items ++ "]"
    where items = map (showStackItem s) . reverse $ gmStack s

showStackItem :: GmState -> Addr -> String
showStackItem s a = show a ++ ": " ++ showNode s a (hLookup (gmHeap s) a)


showNode :: GmState -> Addr -> Node -> String
showNode s a (NNum n) = show n
showNode s a (NBool b) = show b
showNode s a (NNil) = "[]"
showNode s a (NGlobal n g) = "Global " ++ show (head [n | (n,b) <- gmGlobals s, a==b])
   -- Alternative: "Global " ++ show (fst . head . filter ((==a) . snd) $ gmGlobals s)
showNode s a (NAp a1 a2) = "Ap " ++ show a1 ++ " " ++ show a2
showNode s a (NCons a1 a2) = "NCons " ++ show a1 ++ " " ++ show a2
showNode s a (NInd a1)   = "Ind" ++ show a1
showStats :: GmState -> String
showStats s = "Steps taken = " ++ (show . statGetSteps $ gmStats s) ++ "\n" ++ (show (gmOutput s))

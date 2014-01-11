module Main where



import Parser
import Compiler
import Utils
import Evaluator
import Gcode
import Parser

{--
main=let
	a = readFile pfile 
	putStrLn $ showResults(eval $ make)
    where make = compile
--}


main = do
        input <- readFile "pfile"
        print input     
        putStrLn $ showResults(eval  (make input))
		where make input= compile input
		


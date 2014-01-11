module Compiler (compile) where

import Data.List (mapAccumL)

import AssocList
import Heap
import Utils
import Gcode
import Parser

compile :: String -> GmState
compile fc = GmState [] initialCode [] [] heap globals statInitial
    where (heap, globals) = buildInitialHeap fc



buildInitialHeap :: String -> (GmHeap, GmGlobals)
buildInitialHeap fc = mapAccumL allocateSc hInitial compiled
	where compiled = gencpgm (parse fc)



type GmCompiledSC = (Name, Int, GmCode)

allocateSc :: GmHeap -> GmCompiledSC -> (GmHeap, (Name, Addr))
allocateSc heap (name, nargs, instns) = (heap', (name, addr))
    where (heap', addr) = hAlloc heap $ NGlobal nargs instns

initialCode :: GmCode
initialCode = [PUSHGLOBAL "main", EVAL]


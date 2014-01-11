module Heap where

import AssocList

type Heap a = (Int, [Addr], [(Addr, a)])

hInitial :: Heap a
hInitial = (0, map Addr [1..], [])

hAlloc :: Heap a -> a -> (Heap a, Addr)
hAlloc (size, next : free, cts) n = ((size+1, free, (next, n) : cts), next)

hUpdate :: Heap a -> Addr -> a -> Heap a
hUpdate (size, free, cts) a n = (size, free, (a, n) : remove cts a)

hFree :: Heap a -> Addr -> Heap a
hFree (size, free, cts) a = (size-1, a:free, remove cts a)

hLookup :: Heap a -> Addr -> a
hLookup (size,free,cts) a = aLookup cts a $ error msg
    where msg = "can't find node " ++ show a ++ " in heap"

hAddresses :: Heap a -> [Addr]
hAddresses (size,free,cts) = map fst cts 

hSize :: Heap a -> Int
hSize (size,free,cts) = size

hNull :: Addr
hNull = Addr 0

hIsnull :: Addr -> Bool
hIsnull (Addr a) = a == 0

newtype Addr = Addr Int deriving (Eq, Ord)

instance Show Addr where show (Addr a) = '#' : show a

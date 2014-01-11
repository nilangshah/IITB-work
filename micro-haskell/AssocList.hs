module AssocList where
    
import Data.Maybe (fromMaybe)

remove :: Eq a => [(a,b)] -> a -> [(a,b)]
remove []           _   = []
remove (x@(k,_):xs) key = if k == key then xs else x : remove xs key

aLookup :: Eq a => [(a,b)] -> a -> b -> b
aLookup alist find defval = fromMaybe defval $ lookup find alist

aDomain :: ASSOC a b -> [a]
aDomain = map fst

type ASSOC a b = [(a,b)]

import System.IO

main = do
	hSetEcho stdin False
	putStrLn "welcome to Hangman!"
	word<-getLine
	hangman (length(word)) [] (word)
	hSetEcho stdin True


hangman 0 _ _ = putStrLn "You are dead!"
hangman n a b = if ((progress a b) == b) then putStrLn "You won!" else playnext n a b
	where playnext n a b= do
			 	print n
			 	putStrLn "Enter next guess:"
			 	c<-getChar
			 	respond n c a b

respond n c guesses word | c `elem` guesses = do print (progress guesses word)
						 hangman n guesses word
		         | c `elem` word = do print (progress (c:guesses) word)
					      hangman n (c:guesses) word
		         | not (c `elem` guesses) = do print (progress (c:guesses) word)
						       hangman (n-1) (c:guesses) word
		      


progress guesses [] = []
progress guesses (x:xs) = if x `elem` guesses then (x:(progress guesses xs)) else '_': (progress guesses xs)


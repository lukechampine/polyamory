import Data.Char
import System.IO (isEOF, hFlush, stdout)

data Tape = Tape { left  :: [Char]
                 , curr  ::  Char
                 , right :: [Char]
                 }

advTape :: Tape -> Tape
advTape (Tape ls c (r:rs)) = Tape (c:ls) r rs

recTape :: Tape -> Tape
recTape (Tape (l:ls) c rs) = Tape ls l (c:rs)

data BF = BF { program :: Tape
             , array   :: Tape
             }

newBF :: String -> BF
newBF prog = BF (Tape [] (head prog) (tail prog)) (Tape [] '\0' (repeat '\0'))

curVal :: (BF -> Char)
curVal = curr . array

curSym :: (BF -> Char)
curSym = curr . program

advProg :: BF -> BF
advProg bf = bf {program = advTape (program bf)}

recProg :: BF -> BF
recProg bf = bf {program = recTape (program bf)}

advArray :: BF -> BF
advArray bf = bf {array = advTape (array bf)}

recArray :: BF -> BF
recArray bf = bf {array = recTape (array bf)}

jump :: BF -> BF
jump bf
	| sym == '[' && val == '\0' = bf {program = jmpR 0 . program $ bf}
	| sym == ']' && val /= '\0' = bf {program = jmpL 0 . program $ bf}
	| otherwise                 = bf
	where
		(sym, val) = (curSym bf, curVal bf)

jmpR :: Int -> Tape -> Tape
jmpR n tape = case (n, curr tape) of
	(1, ']') -> tape
	(_, ']') -> jmpR (n-1) (advTape tape)
	(_, '[') -> jmpR (n+1) (advTape tape)
	(_, _)   -> jmpR n (advTape tape)

jmpL :: Int -> Tape -> Tape
jmpL n tape = case (n, curr tape) of
	(1, '[') -> tape
	(_, '[') -> jmpL (n-1) (recTape tape)
	(_, ']') -> jmpL (n+1) (recTape tape)
	(_, _)   -> jmpL n (recTape tape)

setCell :: Char -> BF -> BF
setCell c bf = bf {array = (array bf) {curr = c}}

modChar :: Int -> (Char -> Char)
modChar n = chr . (`mod` 256) . (+ n) . ord

interpret :: BF -> IO ()
interpret bf =
	if null . right . program $ bf
	then return ()
	else case curSym bf of
		'>' -> interpret . advProg $ advArray bf
		'<' -> if null . left . array $ bf
			then putStr "Error: out of bounds" >> return ()
			else interpret . advProg $ recArray bf
		'+' -> interpret . advProg $ setCell (modChar (1) (curVal bf)) bf
		'-' -> interpret . advProg $ setCell (modChar (-1) (curVal bf)) bf
		'.' -> do
			putChar . curVal $ bf
			hFlush stdout
			interpret . advProg $ bf
		',' -> do
			c <- getChar
			interpret . advProg $ setCell c bf
		'[' -> interpret . advProg $ jump bf
		']' -> interpret . advProg $ jump bf
		_   -> interpret . advProg $ bf

readBF :: String -> IO String
readBF s = do
	eof <- isEOF
	if eof
	then return s
	else getChar >>= \c -> readBF . (s ++) $ [c]

main :: IO ()
main = readBF "" >>= interpret . newBF >> putChar '\n'

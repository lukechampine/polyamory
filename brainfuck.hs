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

modChar :: Tape -> (Int -> Int) -> Tape
modChar tape f = tape{curr = chr . (`mod` 256) . f . ord $ curr tape}

advance :: Tape -> Tape -> IO ()
advance (Tape _ _ []) _ = return ()
advance program array = interpret (advTape program) array

interpret :: Tape -> Tape -> IO ()
interpret program@(Tape _ '>' _) array = advance program (advTape array)

interpret program@(Tape _ '<' _) array
	| null (left array) = putStr "Error: out of bounds"
	| otherwise         = advance program (recTape array)

interpret program@(Tape _ '+' _) array = advance program (modChar array succ)

interpret program@(Tape _ '-' _) array = advance program (modChar array pred)

interpret program@(Tape _ '.' _) array = do
	putChar (curr array)
	hFlush stdout
	advance program array

interpret program@(Tape _ ',' _) array = do
	eof <- isEOF
	if eof
	then advance program array
	else getChar >>= \c -> advance program array{curr = c}

interpret program@(Tape _ '[' _) array
	| curr array == '\0' = seek program array 0
	| otherwise          = advance program array
	where seek program@(Tape _ c r) array n
		| c == ']' && n == 1 = advance program array
		| c == ']'           = seek (advTape program) array (n-1)
		| c == '['           = seek (advTape program) array (n+1)
		| null r             = putStr "Error: unmatched ["
		| otherwise          = seek (advTape program) array n

interpret program@(Tape _ ']' _) array
	| curr array /= '\0' = seek program array 0
	| otherwise          = advance program array
	where seek program@(Tape l c _) array n
		| c == '[' && n == 1 = advance program array
		| c == '['           = seek (recTape program) array (n-1)
		| c == ']'           = seek (recTape program) array (n+1)
		| null l             = putStr "Error: unmatched ]"
		| otherwise          = seek (recTape program) array n

interpret program array = advance program array

readBF :: IO Tape
readBF = readBF' (Tape [] '\0' []) where
	readBF' tape@(Tape _ _ r) = do
		eof <- isEOF
		if eof
		then return tape
		else getLine >>= \ln -> readBF' tape{right = r ++ ln}

main :: IO ()
main = do
	program <- readBF
	let array = Tape [] '\0' (repeat '\0')
	interpret program array
	putChar '\n'
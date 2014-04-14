import Data.Char
import System.IO (isEOF, hFlush, stdout)

data BF = BF { program :: String
             , prgpointer :: Int
             , array :: [Char]
             , pointer :: Int
             }

newBF :: String -> BF
newBF prog = BF {program = prog, prgpointer = 0, array = repeat '\0', pointer = 0}

curSym :: BF -> Char
curSym bf = program bf !! prgpointer bf

curVal :: BF -> Char
curVal bf = array bf !! pointer bf

adv :: BF -> BF
adv bf = bf {prgpointer = prgpointer bf + 1}

rec :: BF -> BF
rec bf = bf {prgpointer = prgpointer bf - 1}

jump :: BF -> BF
jump bf
	| sym == '[' && val == '\0' = jmpTo ']' (adv bf)
	| sym == ']' && val /= '\0' = jmpTo '[' (rec bf)
	| otherwise                 = bf
	where
		(sym, val) = (curSym bf, curVal bf)

jmpTo :: Char -> BF -> BF
jmpTo c bf
	| curSym bf == (opp c) = jmpTo c . mov c . jmpTo c . mov c $ bf
	| curSym bf == c       = bf
	| otherwise            = jmpTo c . mov c $ bf
	where
		opp '[' = ']'; opp ']' = '['
		mov '[' = rec; mov ']' = adv

setCell :: Char -> BF -> BF
setCell c bf = let (l,r) = splitAt (pointer bf) (array bf)
               in bf {array = l ++ [c] ++ tail r}

modChar :: Int -> (Char -> Char)
modChar n = chr . (`mod` 256) . (+ n) . ord

interpret :: BF -> IO ()
interpret bf =
	if prgpointer bf == length (program bf)
	then return ()
	else case curSym bf of
		'>' -> interpret . adv $ bf {pointer = pointer bf + 1}
		'<' -> interpret . adv $ bf {pointer = pointer bf - 1}
		'+' -> interpret . adv $ setCell (modChar (1) (curVal bf)) bf
		'-' -> interpret . adv $ setCell (modChar (-1) (curVal bf)) bf
		'.' -> do
			putChar . curVal $ bf
			hFlush stdout
			interpret . adv $ bf
		',' -> do
			c <- getChar
			interpret . adv $ setCell c bf
		'[' -> interpret . adv $ jump bf
		']' -> interpret . adv $ jump bf
		_   -> interpret . adv $ bf

readBF :: String -> IO String
readBF s = do
	eof <- isEOF
	if eof
	then return s
	else getChar >>= \c -> readBF . (s ++) $ [c]

main :: IO ()
main = readBF "" >>= interpret . newBF >> putChar '\n'

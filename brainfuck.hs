import Data.Char

data BF = BF { program :: String
             , prgpointer :: Int
             , array :: [Char]
             , pointer :: Int
             }

newBF :: String -> BF
newBF prog = BF {program = prog, prgpointer = 0, array = replicate 30000 '\0', pointer = 0}

curSym :: BF -> Maybe Char
curSym bf
	| prgpointer bf < 0 = Nothing
	| prgpointer bf >= (length . program) bf = Nothing
	| otherwise = Just (program bf !! prgpointer bf)

curVal :: BF -> Maybe Char
curVal bf
	| pointer bf < 0 = Nothing
	| pointer bf >= (length . array) bf = Nothing
	| otherwise = Just (array bf !! pointer bf)

adv :: BF -> BF
adv bf = bf {prgpointer = prgpointer bf + 1}

rec :: BF -> BF
rec bf = bf {prgpointer = prgpointer bf - 1}

jump :: BF -> BF
jump bf
	| sym == Just '[' && val == Just '\0' = jmpTo ']' (adv bf)
	| sym == Just ']' && val /= Just '\0' = jmpTo '[' (rec bf)
	| otherwise                      = bf
	where
		(sym, val) = (curSym bf, curVal bf)

jmpTo :: Char -> BF -> BF
jmpTo c bf
	| curSym bf == Nothing      = bf {prgpointer = -2}
	| curSym bf == Just (opp c) = (jmpTo c . jmpTo c) bf
	| curSym bf == Just c       = bf
	| otherwise                 = (jmpTo c . rec) bf
	where
		opp '[' = ']'
		opp ']' = '['

next :: BF -> BF
next bf = bf {pointer = pointer bf + 1}

prev :: BF -> BF
prev bf = bf {pointer = pointer bf - 1}

updateElem :: Int -> Int -> [Char] -> [Char]
updateElem n s xs = let (l,r) = splitAt n xs
               in l ++ [chr (ord (head r) + s)] ++ tail r

inc :: BF -> BF
inc bf = bf {array = updateElem (pointer bf) 1 (array bf)}

dec :: BF -> BF
dec bf = bf {array = updateElem (pointer bf) (-1) (array bf)}

interpret :: BF -> BF
interpret bf =
	if curSym bf == Nothing
	then bf
	else interpret . adv $ case curSym bf of
		Just '>' -> next bf
		Just '<' -> prev bf
		Just '+' -> inc bf
		Just '-' -> dec bf
		Just '.' -> bf
		Just ',' -> bf
		Just '[' -> jump bf
		Just ']' -> jump bf
		Just _   -> bf

instance Show BF where show = showBF
showBF :: BF -> String
showBF bf = reverse (dropWhile (== '\0') (reverse (array bf)))

main :: IO ()
main = getContents >>= putStrLn . show . interpret . newBF
import Control.Applicative
import Control.Monad
import Data.Tree

data Parser a = P(String -> [(a,String)])

headOption (x:xs) = Just x
headOption [] = Nothing

parse (P m) = m

result m s= case headOption (parse m s) of
	Just(res,"") -> Just(res)
	_ -> Nothing
instance Functor Parser where
	fmap f m = m >>= \a -> return $ f a

instance Monad Parser where
	return a = P (\s -> [(a,s)])
	(>>=) (P m) f = P(\s -> m s >>= \(a,s) -> parse (f a) s)

instance MonadPlus Parser where
	mzero = P(\s -> [])
	mplus (P a) (P b) = P(\s -> (a s) ++ (b s)) 

instance Applicative Parser where
	pure = return
	(<*>) mf ma = mf >>= \f -> ma >>= \a -> return $ f a

instance Alternative Parser where
	empty = mzero
	(<|>) = mplus

zero' _ _ = []
zero a = P(zero' a)

item' (x:xs) = [(x,xs)]
item' [] = []
item = P(item')

term y = P(term' y)
term' y (x:xs) = if(x == y) then [(y,xs)] else [] 
term' y [] = []

word = sequence . map (term) 
digits = some $ msum $ map term ['0'..'9']
integerParser = fmap (read :: String -> Integer) digits
floatParser = fmap (read :: String -> Float) $ fmap join $ sequence [digits,fmap (join) (sequence [word ".",digits]) <|> (return "")]
oper op s = fmap (const $ BinaryOp op) $ word s
unary op s = fmap (const $ UnaryOp op) $ word s

data Token = Val Float | BinaryOp (Float -> Float -> Float) | UnaryOp (Float -> Float)
type AST = Tree Token

node x = Node x []
unaryMin x = -x
unfolder (term1,((op,term2):xs)) = (node op,[(term1,[]),(term2,xs)])
unfolder (term1,[]) = (term1,[])
tokenToString (Val x) = show x
tokenToString _ = "op"
spaces = many $ word " "

between p m = do {p; r <- m; p; return r }

termAndOperations terms ops = do
	spaces
	term <- terms
	op <- ops
	spaces
	return $ join $ unfoldTree unfolder (term,op)

expr = let unaryAndFactors = do { op <- unary unaryMin "-"; term <- eterm; return $ Node op [term] } <|> eterm;
		   additionAndSubstruction = many $ do { op <- oper (+) "+" <|> oper (-) "-"; other <- eterm; return (op,other) } 
		in termAndOperations unaryAndFactors additionAndSubstruction
eterm = let exponent = expterm;
			multAndDiv = many $ do { op <- oper (*) "*" <|> oper (/) "/"; f <- expterm; return (op,f)  }
		in termAndOperations exponent multAndDiv 
expterm = let exponentOp = many $ do {op <- oper (**) "^"; f <- factor; return (op,f)}
		  in termAndOperations factor exponentOp
factor = between spaces $ (fmap (node . Val) floatParser) <|> do { word "("; e <- expr;word ")"; return e; }


compute (Node (BinaryOp(f)) (x:y:[])) = compute(x) `f` compute(y)
compute (Node (UnaryOp(f)) (x:[])) = f $ compute(x)
compute (Node (Val(x)) []) = x

eval = fmap compute . result expr

data InterpeterTestCases a = EqualityOf String (Maybe a)

tests :: [InterpeterTestCases Float]
tests = [
	EqualityOf "2 + 3" $ Just(5),
	EqualityOf "4 - 3" $ Just(1),
	EqualityOf "2 + (-3)" $ Just(-1),
	EqualityOf "4 * 5" $ Just(20),
	EqualityOf "6/4" $ Just(6/4),
	EqualityOf "1.2 + 1/2" $ Just(1.2+1/2),
	EqualityOf "1/(-3)" $ Just(1/(-3)),
	EqualityOf "0.5 + 0.2" $ Just(0.7),
	EqualityOf "3 ^ 2 ^ 2" $ Just(81),
	EqualityOf "17654/342" $ Just(8827/171),
	EqualityOf "2/3^2" $ Just(2/9),
	EqualityOf "(2/3)^2" $ Just(4/9)] 

main = do
	print "(2 + 3) / (2 - 2) == Just(Infinity)"
	print $ Just $ Just(1/0) == eval "(2 + 3) / (2 - 2)"
	print "2 + 345 + + + + 6 == Nothing"
	print $ Just $ Nothing == eval "2 + 345 + + + + 6"  
	forM_ tests $ \(EqualityOf expr expected) -> do 
		print $ expr ++ " == " ++ (show expected)
		let difference = liftA2 (-) (eval expr) expected
		print $ fmap ((<0.0001) . abs) difference
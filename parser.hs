import Control.Applicative
import Control.Monad
import Data.Tree

data Parser a = P(String -> [(a,String)])

headOption (x:xs) = Just x
headOption [] = Nothing

parse (P m) = m

result m = fmap fst . headOption . parse m

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
floatParser = fmap (read :: String -> Float) $ fmap join $ sequence [digits,word ".",digits]
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
expr :: Parser AST
expr = do 
	term <- do { op <- unary unaryMin "-"; term <- eterm; return $ Node op [term] } <|> eterm
	ops <- many $ do { op <- oper (+) "+" <|> oper (-) "-"; other <- eterm; return (op,other) }
	return $ join $ unfoldTree unfolder (term,ops)
eterm :: Parser AST
eterm = do 
	fact <- factor  
	others <- many $ do { op <- oper (*) "*" <|> oper (/) "/"; f <- factor; return (op,f)  }
	return $ join $ unfoldTree unfolder (fact,others)
factor :: Parser AST
factor = (fmap (node . Val) floatParser) <|> do { word "("; e <- expr;word ")"; return e; }

compute (Node (BinaryOp(f)) (x:y:[])) = compute(x) `f` compute(y)
compute (Node (UnaryOp(f)) (x:[])) = f $ compute(x)
compute (Node (Val(x)) []) = x

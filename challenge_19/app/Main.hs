-- Calculator
-- Write a program which will give the answer to numerical expressions.

module Main where

import Control.Monad (forever)
import System.IO (hFlush, stdout)

main :: IO ()
main = forever $ do
        putStr "> "
        hFlush stdout
        line <- getLine
        case fmap parse (tokenize line) of
                (Right (Just a, [])) -> print $ eval a
                (Right (_, token : _)) -> putStrLn $ "Unexpected Token: " ++ show token
                (Right (Nothing, _)) -> putStrLn "Parse Error"
                (Left (InvalidToken str)) -> putStrLn $ "Invalid Token: " ++ str

data Token = Literal Float | LParen | RParen | Add | Sub | Mul | Div deriving (Show)

newtype InvalidToken = InvalidToken String deriving (Show)

tokenize :: String -> Either InvalidToken [Token]
tokenize "" = Right []
tokenize (' ' : str) = tokenize str
tokenize ('+' : str) = (Add :) <$> tokenize str
tokenize ('-' : str) = (Sub :) <$> tokenize str
tokenize ('*' : str) = (Mul :) <$> tokenize str
tokenize ('/' : str) = (Div :) <$> tokenize str
tokenize ('(' : str) = (LParen :) <$> tokenize str
tokenize (')' : str) = (RParen :) <$> tokenize str
tokenize str = case reads str of
        [(num, str')] -> (Literal num :) <$> tokenize str'
        _ -> Left $ InvalidToken str

data Expression
        = LiteralExpr Float
        | BinaryExpression Expression BinaryOperation Expression
        deriving (Show)

data BinaryOperation = AddOp | SubOp | MulOp | DivOp deriving (Show)

bindingPower :: BinaryOperation -> (Int, Int)
bindingPower AddOp = (1, 2)
bindingPower SubOp = (1, 2)
bindingPower MulOp = (2, 3)
bindingPower DivOp = (2, 3)

data BinaryTerm = BinaryTerm Expression (Maybe BinaryOp) deriving (Show)
data BinaryOp = BinaryOp BinaryOperation BinaryTerm deriving (Show)

parseTerm :: [Token] -> (Maybe BinaryTerm, [Token])
parseTerm (Literal num : tokens) =
        let (op, tokens') = parseOp tokens
         in (Just $ BinaryTerm (LiteralExpr num) op, tokens')
parseTerm (LParen : tokens) = case parse tokens of
        (Just term, RParen : tokens') ->
                let (op, tokens'') = parseOp tokens'
                 in (Just $ BinaryTerm term op, tokens'')
        _ -> (Nothing, tokens)
parseTerm tokens = (Nothing, tokens)

parseOp :: [Token] -> (Maybe BinaryOp, [Token])
parseOp (Add : tokens) =
        let (term, tokens') = parseTerm tokens
         in (fmap (BinaryOp AddOp) term, tokens')
parseOp (Sub : tokens) =
        let (term, tokens') = parseTerm tokens
         in (fmap (BinaryOp SubOp) term, tokens')
parseOp (Mul : tokens) =
        let (term, tokens') = parseTerm tokens
         in (fmap (BinaryOp MulOp) term, tokens')
parseOp (Div : tokens) =
        let (term, tokens') = parseTerm tokens
         in (fmap (BinaryOp DivOp) term, tokens')
parseOp tokens = (Nothing, tokens)

precedence :: BinaryTerm -> Expression
precedence (BinaryTerm term Nothing) = term
precedence (BinaryTerm lhs (Just (BinaryOp op (BinaryTerm rhs Nothing)))) =
        BinaryExpression lhs op rhs
precedence (BinaryTerm a (Just (BinaryOp lop (BinaryTerm b (Just (BinaryOp rop c)))))) =
        if lpower > rpower
                then BinaryExpression a lop $ BinaryExpression b rop $ precedence c
                else BinaryExpression (BinaryExpression a lop b) rop (precedence c)
    where
        (_, lpower) = bindingPower lop
        (rpower, _) = bindingPower lop

parse :: [Token] -> (Maybe Expression, [Token])
parse tokens =
        let (term, tokens') = parseTerm tokens
         in (fmap precedence term, tokens')

eval :: Expression -> Float
eval (LiteralExpr n) = n
eval (BinaryExpression a AddOp b) = eval a + eval b
eval (BinaryExpression a SubOp b) = eval a - eval b
eval (BinaryExpression a MulOp b) = eval a * eval b
eval (BinaryExpression a DivOp b) = eval a / eval b

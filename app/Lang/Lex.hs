module Lang.Lex 
    ( lexStr 
    , Token(..)
    , TokTy(..)
    ) where
import qualified Data.Bifunctor as Data
import Control.Applicative
import Data.Foldable (Foldable(foldl'))
import qualified Data.Char as Char
import Parser

type Lexer a = Parser Char a


data Token = Token { 
    tokLexeme :: String,
    tokTy :: TokTy
} deriving (Show)

data TokTy 
    = For 
    | If 
    | Else
    | Colon 
    | SemiColon
    | LCurly 
    | RCurly 
    | LParen 
    | RParen 
    | LSquare 
    | RSquare 
    | Num 
    | Dot 
    | DotDot 
    | Ident 
    | StrConcat 
    | Eq 
    | NEq 
    | StrLit
    deriving(Show, Eq)


lexStr :: String -> Either String [Token] 
lexStr s = case runParser (many tok) s of 
          Right (x, []) -> Right x
          Right (_, c:cs) -> Left $ "Unexpected " ++ [c]
          Left x -> Left x
    where          
        tok = many space *> (keyword <|> grammar <|> literal <|> op) <* many space

as :: Lexer String -> TokTy -> Lexer Token
as l t = (\s -> Token {tokLexeme=s, tokTy=t}) <$> l

keyword :: Lexer Token
keyword = string "if" `as` If <|> string "for" `as` For <|> string "else" `as` Else

grammar :: Lexer Token
grammar = foldl' (<|>) empty ((\(s, t) -> string s `as` t) <$> gToks)
    where 
        gToks = [(":", Colon), (".", Dot), ("..", DotDot), ("(", LParen), 
                 (")", RParen), ("[", LSquare), ("]", RSquare), ("{", LCurly),
                 ("}", RCurly), (";", SemiColon)]

literal :: Lexer Token 
literal = strLit <|> numLit <|> ident
    where 
        strLit = (char '"' *> many (satisfies (/= '"')) <* char '"') `as` StrLit
        numLit = some (satisfies Char.isNumber) `as` Num
        ident = (((:) <$> (char '_' <|> satisfies Char.isAlpha)) <*> many (char '_' <|> satisfies Char.isAlphaNum)) `as` Ident

op :: Lexer Token
op = string "++" `as` StrConcat <|> string "==" `as` Eq <|> string "!=" `as` NEq

space :: Lexer Char
space = satisfies Char.isSpace

string :: String -> Lexer String 
string = traverse char


char :: Char -> Lexer Char 
char = satisfies . (==)



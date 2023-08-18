{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE LambdaCase #-}
module Lang.Parser ( parse
                   ) where

import qualified Lang.Lex as L
import Lang.Ast
import Data.Bifunctor
import Control.Applicative (Alternative(..))
import Data.Maybe (fromMaybe)
import Data.Foldable (Foldable(foldl'))
import Parser

type TParser = Parser L.Token

as :: TParser L.Token -> (String -> b) -> TParser b
as t f = f . L.tokLexeme <$> t

parse :: [L.Token] -> Either String Ast 
parse s =  
    case runParser program s of 
        Right(a, []) -> Right a 
        Right (_, c:cs) -> Left $ "Could not parse " ++ L.tokLexeme c ++ " (are you forgetting a semicolon?)"
        Left err -> Left err

program :: TParser Ast 
program = Block <$> many statement

statement :: TParser Ast 
statement = controlFlow <|> fullExpr

controlFlow :: TParser Ast 
controlFlow = forStmt <|> ifStmt 
    where 
        forStmt = do 
            ident <- tok L.For *> tok L.Ident 
            expr <- tok L.Colon *> expression
            body <- block
            return $ For {forVar = L.tokLexeme  ident, forIn = expr, forBody = body}

        ifStmt = do
            expr <- tok L.If *> expression
            body <- block
            rest <- maybeParse (tok L.Else *> block <|> tok L.Else *> ifStmt)
            return $ If {ifCond=expr, ifTrue=body, ifFalse=rest}


        block = Block <$> between L.LCurly L.RCurly (many statement)

fullExpr :: TParser Ast 
fullExpr = expression <* tok L.SemiColon 

expression :: TParser Ast
expression = lhs `andMaybe` rhs
    where
        andMaybe p1 p2 = p1 <|> (p1 >>= p2)
        lhs = ((Lit <$> literal) <|> between L.LParen L.RParen expression) `andMaybe` postFixOp
        rhs = (<*> expression) . mkBinOp
        mkBinOp a = (\ty rhs -> BinOp {opLhs=a, opRhs=rhs, opTy=ty}) <$> binOp

postFixOp :: Ast -> TParser Ast
postFixOp a = nameLookup
    where
        nameLookup = foldl' NameLookup a <$> getNames
        getNames = fmap L.tokLexeme <$> some (tok L.Dot *> tok L.Ident)

binOp :: TParser OpTy
binOp = (StrConcat <$ tok L.StrConcat) <|> (Eq <$ tok L.Eq) <|> (NEq <$ tok L.NEq)

literal :: TParser Literal
literal = tok L.Ident `as` Ident <|> tok L.StrLit `as` StrLit

between :: L.TokTy -> L.TokTy -> TParser a -> TParser a
between lhs rhs p = tok lhs *> p <* tok rhs

tok :: L.TokTy -> TParser L.Token
tok t = satisfies ((==) t . L.tokTy)



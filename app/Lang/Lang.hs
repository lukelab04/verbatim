{-# LANGUAGE FlexibleInstances #-}
module Lang.Lang ( evaluate
                 , Literal(..)
                 , IntoLiteral(..)
                 ) where

import Lang.Parser(parse)
import Lang.Lex(lexStr)
import Lang.Ast
import Control.Arrow(Arrow(second))

class IntoLiteral a where
    intoLit :: a -> Literal

instance IntoLiteral Literal where 
    intoLit l = l

instance IntoLiteral String where
    intoLit = StrLit

instance {-# OVERLAPS #-} IntoLiteral a => IntoLiteral [(String, a)] where
    intoLit = Document . fmap (second intoLit)

instance {-# OVERLAPS #-} IntoLiteral a => IntoLiteral [a] where
    intoLit = List . fmap intoLit

evaluate :: String -> [(String, Literal)] -> Either String String
evaluate s m = eval m <$> (lexStr s >>= parse)


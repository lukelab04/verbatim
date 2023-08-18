{-# LANGUAGE LambdaCase #-}
module Lang.Ast ( Ast(..)
                , Literal(..)
                , OpTy(..)
                , eval
                ) where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Foldable (Foldable(foldl'))

data Ast
    = If { ifCond :: Ast
         , ifTrue :: Ast
         , ifFalse :: Maybe Ast
         }
    | For { forVar :: String
          , forIn :: Ast
          , forBody :: Ast
          }
    | Block [Ast]
    | NameLookup Ast String
    | BinOp { opLhs :: Ast
            , opRhs :: Ast
            , opTy :: OpTy
            }
    | Lit Literal
    deriving(Show)

data Literal
    = StrLit String
    | Document [(String, Literal)]
    | List [Literal]
    | Ident String
    deriving(Show)

instance Eq Literal where
    l1 == l2 =
        case (l1, l2) of 
            (StrLit a, StrLit b) -> a == b
            (Document a, Document b) -> a == b
            (List a, List b) -> a == b 
            (Ident a, Ident b) -> a == b 
            _ -> False

stringify :: Env -> Literal -> String
stringify e =
    \case
        StrLit s -> s
        Document d -> foldl' (++) "" (stringify e . snd <$> d)
        List l -> foldl' (++) "" (stringify e <$> l)
        Ident s -> stringify e $ mGet e s

data OpTy
    = StrConcat
    | Eq
    | NEq
    deriving(Show)


type Env = Map.Map String Literal

mGet :: Env -> String -> Literal
mGet e s = fromMaybe (StrLit "") (Map.lookup s e)

mPut :: Env -> String -> Literal -> Env
mPut e name val = Map.insert name val e

eval :: [(String, Literal)] -> Ast -> String 
eval m = let map = Map.fromList m
         in stringify map . eval' map

eval' :: Env -> Ast -> Literal
eval' e =
    \case
        If cond t f -> if truthy $ eval' e cond
                       then eval' e t
                       else maybe (StrLit "") (eval' e) f

        For ident expr body ->
            let envs = mPut e ident <$> iter (eval' e expr)
                vals = (`eval'` body) <$> envs
            in StrLit $ foldl' (++) "" (stringify e <$> vals)
        Block b -> StrLit $ foldl' (++) "" (stringify e . eval' e <$> b)
        NameLookup obj name -> docGet name $ eval' e obj
        BinOp lhs rhs ty ->
            let lhsVal = eval' e lhs
                rhsVal = eval' e rhs
            in case ty of
                StrConcat -> StrLit $ stringify e lhsVal ++ stringify e rhsVal
                Eq -> if lhsVal == rhsVal then StrLit "true" else StrLit ""
                NEq -> if lhsVal /= rhsVal then StrLit "true" else StrLit ""
        Lit l -> l


    where
        truthy :: Literal -> Bool
        truthy = 
            \case 
                StrLit s -> not $ null s
                Document d -> not $ null d 
                List l -> not $ null l 
                Ident i -> truthy $ mGet e i

        iter :: Literal -> [Literal]
        iter = 
            \case 
                s@(StrLit _) -> [s]
                Document d -> snd <$> d 
                List l -> l 
                Ident i -> iter $ mGet e i

        docGet name =
            \case
                Document d -> fromMaybe (StrLit "") (lookup name d)
                Ident i -> docGet name $ mGet e i
                other -> StrLit ""


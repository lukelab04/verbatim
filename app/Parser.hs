{-# LANGUAGE LambdaCase #-}
module Parser( Parser(..)
             , satisfies 
             , maybeParse
             , withFallback
             ) where 
import qualified Data.Bifunctor
import Control.Applicative

newtype Parser s a = Parser {
    runParser :: [s] -> Either String (a, [s])
}

instance Functor (Parser a) where
    fmap f (Parser p) = Parser (fmap (Data.Bifunctor.first f) . p)

instance Applicative (Parser a) where
    pure a = Parser $ \t -> Right (a, t)
    (Parser p1) <*> (Parser p2) = Parser $ \t -> do
        (out, rest) <- p1 t
        (out', rest') <- p2 rest
        Right (out out', rest')

instance Alternative (Parser a) where
    empty = Parser $ \t -> Left "Empty Parser"
    (Parser p1) <|> (Parser p2) = Parser $ \t ->
        case (p1 t, p2 t) of
          (Right a, Right b) -> if length (snd a) <= length (snd b) then Right a else Right b
          (x@(Right _), _) -> x
          (_, x@(Right _)) -> x
          (x@(Left _), _) -> x

instance Monad (Parser a) where
    (Parser p1) >>= f = Parser $ \t -> do
        (out, rest) <- p1 t
        runParser (f out) rest



satisfies :: Show s => (s -> Bool) -> Parser s s
satisfies p = Parser $
    \case
        t:ts | p t -> Right (t, ts)
        t:ts -> Left $ "Unexpected input " ++ show t
        _ -> Left "Unexpected EOF"



maybeParse :: Parser s a -> Parser s (Maybe a)
maybeParse (Parser p) = Parser $ \t ->
    case p t of
      Left _ -> Right (Nothing, t)
      Right (a, b) -> Right (Just a, b)


withFallback :: Parser s a -> Parser s a -> Parser s a 
withFallback (Parser p1) (Parser p2) = Parser $ \t -> 
    case p1 t of
        Left _ -> p2 t 
        Right x -> Right x

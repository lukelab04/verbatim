module JSON ( parseJson ) where
import qualified Data.Bifunctor
import Parser
import qualified Data.Char as Char
import Lang.Lang
import Control.Applicative
import Data.Maybe (fromMaybe)
import Data.Functor (($>))
import qualified Debug.Trace as Debug


type JParser a = Parser Char a

parseJson :: String -> Either String Literal 
parseJson s = 
    case runParser value s of 
        Right (l, []) -> Right l 
        Right (_, s) -> Left $ "Could not parse " ++ s 
        Left s -> Left s

value :: JParser Literal
value = many space *> ((object
      <|> array
      <|> (intoLit <$> string))
      `withFallback` other)
      <* many space

object :: JParser Literal
object = ((char '{' *> many space *> char '}') $> Document [])
         <|> (char '{' *> members <* char '}')
    where
        members :: JParser Literal
        members = intoLit <$> (((\x -> (++) [x]) <$> member) <*> many (char ',' *> member))

        member :: JParser (String, Literal)
        member = (many space *> ((,) <$> string) <* (many space <* char ':')) <*> element


array :: JParser Literal
array = ((char '[' *> many space *> char '}') $> List [])
        <|> intoLit <$> (char '[' *> elements <* char ']')


other :: JParser Literal 
other = intoLit <$> some (satisfies (\c -> Char.isAlphaNum c || c == '-' || c == '+' || c == '.'))


string :: JParser String
string = char '"' *> many (satisfies (/= '"')) <* char '"'

elements :: JParser [Literal]
elements = ((\x -> (++) [x]) <$> element) <*> many (char ',' *> element)

element :: JParser Literal
element = many space *> value <* many space


char :: Char -> JParser Char
char = satisfies . (==)

space = satisfies Char.isSpace


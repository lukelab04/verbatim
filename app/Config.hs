
{-# LANGUAGE LambdaCase #-}
module Config ( Config(..)
              , getConfig
              ) where

import Data.Functor ((<$>))
import Control.Arrow
import Control.Applicative
import qualified Data.Char as Char
import Data.Map
import System.FilePath
import Data.List (dropWhileEnd)
import Parser

data Config
    = Config { configSourceDir :: String
             , configBuildDir :: String
             , configServerRoute :: String
             , configTemplateDir :: String
             , configAdditionalFiles :: Maybe String
             , configBlogName :: Maybe String
             }

cleanPath :: String -> String
cleanPath s = dropWhile Char.isSpace (dropWhileEnd Char.isSpace s)

getConfig :: String -> IO Config 
getConfig absPath = do 
    contents <- readFile (absPath </> "verbatim.config")
    return $ parseConfig absPath contents

parseConfig :: String -> String -> Config
parseConfig absPath contents =
    let ops = (\case Left x -> error x; Right x -> fst x) (runParser file contents)
        ; map = Data.Map.fromList ops
    in Config { configSourceDir = cleanPath $ absPath </> mapGet "SourceDirectory" map
              , configBuildDir = cleanPath $ absPath </> mapGet "BuildDirectory" map
              , configServerRoute = cleanPath $ mapGet "ServerRoute" map
              , configTemplateDir = cleanPath $ absPath </> mapGet "TemplateDirectory" map
              , configAdditionalFiles = (\s -> cleanPath $ absPath </> s) <$> Data.Map.lookup "AdditionalFiles" map
              , configBlogName = cleanPath <$> Data.Map.lookup "BlogName" map
              }
    where
        mapGet name map =
            case Data.Map.lookup name map of
                Nothing -> error "Necessary option '" ++ name ++ "' was not found in verbatim.config."
                Just x -> x

type Lexer a = Parser Char a

file :: Lexer [(String, String)]
file = many line

line :: Lexer (String, String)
line = ((,) <$> (spaces *> name)) <*> (many (space <|> char '=') *> many (satisfies (/= '\n')))
    where
        space = satisfies Char.isSpace
        spaces = many space
        name = some $ satisfies Char.isAlpha

string :: String -> Lexer String
string = traverse char

char :: Char -> Lexer Char
char c = satisfies (== c)


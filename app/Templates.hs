module Templates ( getTemplateMap 
                 , fillTemplates 
                 ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory
import System.FilePath
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Bifunctor
import qualified Lang.Ast as Lit
import Lang.Lang(evaluate, Literal, IntoLiteral (intoLit))
import Files.Files
import Markdown
import Config

getTemplateMap :: FilePath -> IO [(String, T.Text)]
getTemplateMap f = do
    content <- listDirectory f
    text <- sequence $ TIO.readFile . (</>) f <$> content
    let templates = zip (takeBaseName <$> content) text
    return $ Data.Bifunctor.second (fillTemplate templates) <$> templates


fillTemplate :: [(String, T.Text)] -> T.Text -> T.Text
fillTemplate tMap t =
    case findHole t of
        Just (h, n, t) -> fillTemplate tMap (h <> fromMaybe T.empty (lookup n tMap) <> t)
        Nothing -> t
    where
        -- Find <!--Template:Name-->, return text before comment, 'Name', text after comment
        findHole :: T.Text -> Maybe (T.Text, String, T.Text)
        findHole t =
            case T.breakOn (T.pack "<!--Template:") t of
                (first, rest) | not $ T.null rest ->
                    let name = T.takeWhile (/= '-') (T.drop 1 (T.dropWhile (/= ':') rest))
                        end = T.drop 1 (T.dropWhile (/= '>') rest)
                    in Just (first, T.unpack name, end)
                _ -> Nothing


runScripts :: Config -> T.Text -> Literal -> T.Text
runScripts config input val =
    case T.breakOn (T.pack "$$") input of
        (first, rest) | not $ T.null rest ->
            let code = T.takeWhile (/= '$') (T.drop 2 rest)
                rest' = T.drop 2 $ T.dropWhile (/= '$') $ T.drop 2 rest
            in runScripts config (first <> T.pack (runScript (T.unpack code)) <> rest') val
        _ -> input
    where
        runScript c =
            case evaluate c [("document", val), globalOpts] of
                Right x -> x
                Left e -> error $ "Script runtime error: " ++ e
        
        globalOpts = ("blog", intoLit [ ("name", fromMaybe "Blog" (configBlogName config))
                              , ("root", configServerRoute config)
                              ])


fillTemplates :: Config -> [(String, T.Text)] -> Entry -> T.Text
fillTemplates config templateMap e =
    let props = getProps e
        template = case e of
                    Directory _ _ isRoot | isRoot && isJust (lookup "Home" templateMap) -> 
                        case lookup "Home" templateMap of Just x -> x; _ -> error "Haskell error"
                    Directory {} -> 
                        case lookup "Directory" templateMap of 
                            Just x -> x 
                            _ -> T.pack ""
                    Document {} -> case lookup "Document" templateMap of 
                                        Just x -> x 
                                        _ -> T.pack ""
                    NonContent _ -> T.pack ""
    in runScripts config template props



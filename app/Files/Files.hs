{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list comprehension" #-}
{-# LANGUAGE LambdaCase #-}
module Files.Files ( buildFiles
                   , getProps
                   , Entry(..)
                   , setOutput
                   , writeFiles
                   , copyFolder
                   ) where

import System.FilePath
import System.Directory
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock
import Lang.Lang(Literal, intoLit)
import qualified Lang.Ast as Lit
import JSON
import Data.Foldable
import qualified Debug.Trace as Debug
import Markdown

data Entry
    = Directory
        { dirRelPath :: FilePath
        , dirContents :: [Entry]
        , isRoot :: Bool
        }
    | Document
        { docRelPath :: FilePath
        , docContent :: T.Text
        , docLastUpdated :: UTCTime
        , docProps :: T.Text
        }
    | NonContent FilePath
    deriving (Show)

getProps :: Entry -> Literal
getProps =
    \case
        Directory path dirContents _ -> 
            let entries = intoLit $ getProps <$> dirContents
            in intoLit [("title", intoLit $ takeBaseName path), ("entries", entries), ("relLink", intoLit $ getRelLink path)]
        Document path c _ props -> 
            case parseProps props of 
                Lit.Document m -> 
                    intoLit $ [("content", intoLit $ T.unpack $ compileText c), ("relLink", intoLit $ getRelLink path)] ++ m
                l -> l
        NonContent _ -> intoLit ""

    where
        parseProps :: T.Text -> Literal
        parseProps t | T.null t = intoLit ""
        parseProps t =
            case parseJson $ T.unpack t of
                Right x -> x
                Left e -> error $ "JSON error: " ++ e

        
        getRelLink :: String -> String
        getRelLink "" = "Home.html"
        getRelLink s = "./" ++ takeBaseName s ++ ".html" 

data Files = Files
    { tree :: Entry
    , rootPath :: FilePath
    , toUpdate :: [FilePath]
    , output :: [(FilePath, T.Text)]
    } deriving (Show)


writeFiles :: FilePath -> Files -> IO ()
writeFiles root f = sequence_ $ write <$> output f
    where
        write (path, text) = do
            let fp = root </> path ++ ".html"
            createDirectoryIfMissing True (takeDirectory fp)
            TIO.writeFile fp text


buildFiles :: FilePath -> IO Files
buildFiles f = do
    entry <- buildTree f ""
    return Files { tree=entry, rootPath=f, toUpdate=[], output=[]}
    where
        
        buildTree root rel = do
            let isMd = takeExtension rel == ".md"
            isDir <- doesDirectoryExist $ root </> rel

            if isMd
            then do
                content <- TIO.readFile $ root </> rel
                let (content', props) = getProps content
                mod <- getModificationTime $ root </> rel
                return Document {docRelPath=rel, docContent=content', docLastUpdated=mod, docProps=props}
            else if isDir
            then do
                contents <- listDirectory (root </> rel)
                entries <- sequence $ buildTree root <$> ((</>) rel . takeFileName <$> contents)
                return Directory {dirRelPath=rel, dirContents=entries, isRoot=null rel}
            else return $ NonContent rel


        getProps t =
            if T.take 3 t == T.pack "$$$"
            then let props = T.takeWhile (/= '$') (T.drop 3 t)
                     rest = T.drop 3 $ T.dropWhile (/= '$') $ T.drop 3 t
                 in (rest, props)
            else (t, T.empty)


setOutput :: (Entry -> T.Text) -> Files -> Files
setOutput f files = files{output=getOutput f (tree files)}
    where
        getOutput :: (Entry -> T.Text) -> Entry -> [(String, T.Text)]
        getOutput f tree =
            case tree of
                Directory dirRelPath dirContents _ ->
                    let path = if null dirRelPath then "Home" else dirRelPath
                    in (dropExtension path, f tree) : concat (getOutput f <$> dirContents)
                Document docRelPath docContent _ _ ->
                    if docRelPath `elem` toUpdate files || True
                    then [(dropExtension docRelPath, f tree)]
                    else []
                NonContent _ -> []


copyFolder :: FilePath -> FilePath -> IO () 
copyFolder src dst = copyFolder' src dst ""

copyFolder' :: FilePath -> FilePath -> FilePath -> IO () 
copyFolder' src dst rel = do 
    content <- listDirectory (src </> rel)
    writeElem (head content)

    where 
        writeElem :: FilePath -> IO ()
        writeElem f = do 
            isDir <- doesDirectoryExist (src </> rel </> f)
            if isDir 
            then copyFolder' src dst (rel </> f)
            else do 
                createDirectoryIfMissing True (takeDirectory $ dst </> rel </> f)
                copyFile (src </> rel </> f) (dst </> rel </> f)




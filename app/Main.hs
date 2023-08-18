module Main where

import System.Directory
import Files.Files
import Templates
import Config
import GHC.IO.Encoding (setLocaleEncoding, utf8)

main :: IO ()
main = do 
    setLocaleEncoding utf8 
    config <- getConfig =<< getCurrentDirectory
    files <- buildFiles $ configSourceDir config
    templateMap <- getTemplateMap $ configTemplateDir config

    let files' = setOutput (fillTemplates config templateMap) files
    writeFiles (configBuildDir config) files'

    case configAdditionalFiles config of 
         Just f -> copyFolder f (configBuildDir config)
         _ -> return ()
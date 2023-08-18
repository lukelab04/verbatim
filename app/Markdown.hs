
module Markdown
    ( compileText
    ) where

import Text.Pandoc
import Text.Pandoc.Options
import Text.Pandoc.Extensions
import Data.Text as T
import qualified Data.Set

addedReadExts = [ Ext_header_attributes 
            , Ext_fenced_code_blocks
            , Ext_fenced_code_attributes
            , Ext_all_symbols_escapable 
            , Ext_strikeout 
            , Ext_superscript 
            , Ext_subscript 
            , Ext_tex_math_dollars 
            , Ext_raw_html 
            , Ext_footnotes 
            , Ext_inline_notes 
            ]
readOps = def { readerExtensions=readerExtensions def <> extensionsFromList addedReadExts
              , readerIndentedCodeClasses=readerIndentedCodeClasses def <> (T.pack <$> ["code-block"])}

writeOps = def {writerHTMLMathMethod=MathJax T.empty }

mapErr :: Show a => Either a b -> b
mapErr e =
    case e of 
        Right x -> x
        Left x -> error $ show x

compileText :: T.Text -> T.Text
compileText s = mapErr . runPure $ readMarkdown readOps s >>= writeHtml5String writeOps

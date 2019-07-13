module Rosy.PreProcessor where

import System.Directory
import System.IO

import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc

import Text.PrettyPrint

preprocessor :: FilePath -> FilePath -> IO ()
preprocessor from to = do
    copyFile from to
    fromhs <- parseFile from
    appendInstances to (moduleDatas fromhs)

parseFile :: FilePath -> IO (Module SrcSpanInfo)
parseFile fp = do
    str <- readFile fp
    case parse str of
        ParseOk code -> return code
        ParseFailed l str -> error $ show l ++ ": " ++ show str

moduleDatas :: Module SrcSpanInfo -> [Doc]
moduleDatas (Module _ _ _ _ decls) = concatMap declDatas decls
moduleDatas _ = []

declDatas :: Decl SrcSpanInfo -> [Doc]
declDatas (DataDecl _ _ _ h _ _) = [prettyPrimWithMode (defaultMode { layout = PPNoLayout }) h]
declDatas _ = []

appendInstances :: FilePath -> [Doc] -> IO ()
appendInstances fp datas = do
    let code = generateInstances datas
    appendFile fp (show code)

generateInstances :: [Doc] -> Doc
generateInstances = foldr (\d code -> generateDataInstances d $+$ code) mempty 

generateDataInstances :: Doc -> Doc
generateDataInstances name
    =   text "\n"
    $+$ text "deriving instance Typeable" <+> name
    $+$ text "deriving instance Generic" <+> name
    $+$ text "instance {-# OVERLAPPABLE #-} Default" <+> name
    $+$ (text "instance {-# OVERLAPPABLE #-} Subscribed" <+> name <+> text "where"
        $+$ nest 5 (text "subscribed = subscribedMemory"))
    $+$ (text "instance {-# OVERLAPPABLE #-} Published" <+> name <+> text "where"
        $+$ nest 5 (text "published = publishedMemory"))
    
    


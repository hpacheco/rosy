module Rosy.PreProcessor where

import System.Directory
import System.IO
import System.Exit

import Language.Haskell.Exts (readExtensions)
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc

import Text.PrettyPrint

import Data.List as List
import Data.Typeable

import Control.Monad
import Control.Exception

import Control.Applicative hiding ((<>))

data PreProcessorException = PreProcessorException String Int Int Doc
  deriving Show
instance Exception PreProcessorException where
    toException = SomeException
    fromException (SomeException x) = cast x

prettyDoc :: Pretty a => a -> Doc
prettyDoc = prettyPrimWithMode (defaultMode { layout = PPNoLayout }) 

preprocessor :: String -> FilePath -> FilePath -> IO ()
preprocessor name from to = catch (runPreprocessor name from to) $ \(PreProcessorException name l c msg) -> do
    die $ show $ text name <> char ':' <> text (show l) <> char ':' <> text (show c) <> char ':' $+$ nest 5 msg

runPreprocessor :: String -> FilePath -> FilePath -> IO ()
runPreprocessor name from to = do
    copyFile from to
    fromhs <- parseFile name from
    appendInstances to (moduleDatas fromhs) (moduleDefaults fromhs)

parseFile :: String -> FilePath -> IO (Module SrcSpanInfo)
parseFile name fp = do
    str <- readFile fp
    let mb = readExtensions str
    let mblang = join $ fmap fst mb
    let exts = maybe [] snd mb
    let mode' = defaultParseMode { extensions = exts }
    let mode'' = case mblang of { Nothing -> mode'; Just lang -> mode' {baseLanguage = lang } }
    let res = parseWithMode mode'' str
    case res of
        ParseOk code -> return code
        ParseFailed l str -> throwIO $ PreProcessorException name (srcLine l) (srcColumn l) (text str)

declDefaults :: Decl SrcSpanInfo -> [Doc]
declDefaults (InstDecl _ _ r _) = instRuleDefaults r
declDefaults _ = []

instRuleDefaults :: InstRule SrcSpanInfo -> [Doc]
instRuleDefaults (IParen _ r) = instRuleDefaults r
instRuleDefaults (IRule _ _ _ h) = instHeadDefaults h

instHeadDefaults :: InstHead SrcSpanInfo -> [Doc]
instHeadDefaults (IHParen _ h) = instHeadDefaults h
instHeadDefaults (IHApp _ h t) = if isDefaultInstHead h then [prettyDoc $ noParenType t] else []
instHeadDefaults _ = []

noParenType :: Type l -> Type l
noParenType (TyParen _ t) = t
noParenType t = t

isDefaultInstHead :: InstHead SrcSpanInfo -> Bool
isDefaultInstHead (IHParen _ h) = isDefaultInstHead h
isDefaultInstHead (IHCon _ qn) = isDefaultQName qn
isDefaultInstHead _ = False

isDefaultQName :: QName l -> Bool
isDefaultQName (UnQual _ n) = isDefaultName n
isDefaultQName _ = False

isDefaultName :: Name l -> Bool
isDefaultName (Ident _ "Default") = True
isDefaultName _ = False

moduleDatas :: Module SrcSpanInfo -> [Doc]
moduleDatas (Module _ _ _ _ decls) = concatMap declDatas decls
moduleDatas _ = []

moduleDefaults :: Module SrcSpanInfo -> [Doc]
moduleDefaults (Module _ _ _ _ decls) = concatMap declDefaults decls
moduleDefaults _ = []

declDatas :: Decl SrcSpanInfo -> [Doc]
declDatas (DataDecl _ _ _ h _ _) = [prettyDoc h]
declDatas _ = []

appendInstances :: FilePath -> [Doc] -> [Doc] -> IO ()
appendInstances fp datas defs = do
    let code = generateInstances defs datas
    appendFile fp (show code)

generateInstances :: [Doc] -> [Doc] -> Doc
generateInstances defs = foldr (\d code -> generateDataInstances defs d $+$ code) mempty 

generateDataInstances :: [Doc] -> Doc -> Doc
generateDataInstances defs name
    =   text "\n"
    $+$ text "deriving instance Typeable" <+> name
    $+$ text "deriving instance Generic" <+> name
    $+$ (if List.elem name defs then text "" else text "instance {-# OVERLAPPABLE #-} Default" <+> name)
    $+$ (text "instance Subscribed" <+> name <+> text "where"
        $+$ nest 5 (text "subscribed = subscribedMemory"))
    $+$ (text "instance Published" <+> name <+> text "where"
        $+$ nest 5 (text "published = publishedMemory"))
    
    


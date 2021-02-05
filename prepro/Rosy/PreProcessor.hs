{-# LANGUAGE CPP #-}

module Rosy.PreProcessor where

import System.Directory
import System.IO
import System.Exit

import Language.Haskell.Exts (readExtensions,KnownExtension(..),Extension(..))
import Language.Haskell.Exts.Parser
import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Pretty
import Language.Haskell.Exts.SrcLoc

import Text.PrettyPrint

import Data.List as List
import Data.Typeable

import Control.Monad
import Control.Exception

#if __GLASGOW_HASKELL__ >= 710
import Prelude hiding ((<>))
#else
import Prelude
#endif

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
    txt <- readFile from
    let pragmas =
            "{-# LANGUAGE ConstraintKinds, RebindableSyntax, PartialTypeSignatures, DataKinds, TypeFamilies, MultiParamTypeClasses, UndecidableInstances, FlexibleInstances #-}\n"
--            "{-# OPTIONS_GHC -fplugin=Type.Compare.Plugin #-}\n"
    fromhs <- parseFile name from
    let (header,decls) = moduleSplit fromhs
    writeFile to pragmas
    appendFile to (header ++ "\n")
    appendFile to "import Prelude\n"
    appendFile to decls
    appendInstances to (moduleDatas fromhs) (moduleDefaults fromhs)

parseFile :: String -> FilePath -> IO (Module SrcSpanInfo)
parseFile name fp = do
    str <- readFile fp
    let mb = readExtensions str
    let mblang = join $ fmap fst mb
    let exts = maybe [] snd mb
    let mode' = defaultParseMode { extensions = EnableExtension DataKinds : exts }
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

moduleDatas :: Module SrcSpanInfo -> [(Doc,Doc,[Doc])]
moduleDatas (Module _ _ _ _ decls) = concatMap declDatas decls
moduleDatas _ = []

moduleSplit :: Module SrcSpanInfo -> (String,String)
moduleSplit (Module l h ps is ds) = (prettyPrint (Module l h ps is []),unlines $ map prettyPrint ds)

moduleDefaults :: Module SrcSpanInfo -> [Doc]
moduleDefaults (Module _ _ _ _ decls) = concatMap declDefaults decls
moduleDefaults _ = []

declDatas :: Decl SrcSpanInfo -> [(Doc,Doc,[Doc])]
declDatas (DataDecl _ _ _ h _ _) = [(prettyDoc h,declHeadName h,declHeadVars h)]
declDatas _ = []

declHeadName :: DeclHead SrcSpanInfo -> Doc
declHeadName (DHead _ n) = prettyDoc n
declHeadName (DHInfix _ v n) = prettyDoc n
declHeadName (DHParen _ d) = declHeadName d
declHeadName (DHApp _ d v) = declHeadName d

declHeadVars :: DeclHead SrcSpanInfo -> [Doc]
declHeadVars (DHead _ _) = []
declHeadVars (DHInfix _ v _) = [prettyDoc v]
declHeadVars (DHParen _ d) = declHeadVars d
declHeadVars (DHApp _ d v) = prettyDoc v : declHeadVars d

appendInstances :: FilePath -> [(Doc,Doc,[Doc])] -> [Doc] -> IO ()
appendInstances fp datas defs = do
    let code = generateInstances defs datas
    let ifthenelse = text "ifThenElse True x y = x"
                 $+$ text "ifThenElse False x y = y"
    appendFile fp (show $ code $+$ ifthenelse)

generateInstances :: [Doc] -> [(Doc,Doc,[Doc])] -> Doc
generateInstances defs = foldr (\d code -> generateDataInstances defs d $+$ code) mempty 

generateInstCtx :: Doc -> [Doc] -> Doc
generateInstCtx n xs = parens $ sepBy (text ",") (map (n <+>) xs)

sepBy :: Doc -> [Doc] -> Doc
sepBy s [] = empty
sepBy s [x] = x
sepBy s (x:xs) = x <> s <> sepBy s xs

generateDataInstances :: [Doc] -> (Doc,Doc,[Doc]) -> Doc
generateDataInstances defs (name,hname,vars)
    =   text "\n"
    $+$ text "deriving instance Typeable" <+> hname
    $+$ text "deriving instance Generic" <+> parens name
    $+$ (if null vars then text "" else text "deriving instance Generic1" <+> hname)
    $+$ (if List.elem name defs
            then text ""
            else text "instance {-# OVERLAPPABLE #-} " <+> defCtx <+> text " => Default" <+> parens name)
    $+$ (text "instance " <+> subCtx <+> text " => Subscribed" <+> parens name <+> text "where"
        $+$ nest 5 (text "subscribed = subscribedEvent"))
    $+$ (text "instance " <+> pubCtx <+> text " => Published" <+> parens name <+> text "where"
        $+$ nest 5 (text "published = publishedEvent"))
 where
   defCtx = generateInstCtx (text "Default") vars
   typCtx = generateInstCtx (text "Typeable") vars
   subCtx = parens $ typCtx <> text "," <> generateInstCtx (text "Subscribed") vars
   pubCtx = parens $ typCtx <> text "," <> generateInstCtx (text "Published") vars
    
    


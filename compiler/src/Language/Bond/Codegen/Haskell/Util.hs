module Language.Bond.Codegen.Haskell.Util where

import Data.Char
import Language.Bond.Codegen.TypeMapping (MappingContext(..), NamespaceMapping(..))
import Language.Bond.Syntax.Types
import Language.Haskell.Exts hiding (Namespace)
import Language.Haskell.Exts.SrcLoc (noLoc)
import Control.Applicative
import Data.List
import Data.Maybe

data CodegenOpts = CodegenOpts
    { setType :: String
    , deriveShow :: Bool
    , deriveEq :: Bool
    }

unique :: Ord a => [a] -> [a]
unique = map head . group . sort

internalModuleName :: ModuleName
internalModuleName = ModuleName "Data.Bond.Internal.Imports"

internalModuleAlias :: ModuleName
internalModuleAlias = ModuleName "B'"

preludeAlias :: ModuleName
preludeAlias = ModuleName "P'"

capitalize :: String -> String
capitalize (h : t) = toUpper h : t
capitalize "" = ""

uncapitalize :: String -> String
uncapitalize (h : t) = toLower h : t
uncapitalize "" = ""

unqual :: String -> QName
unqual = UnQual . Ident

mkVar :: String -> Name
mkVar = Ident . uncapitalize

mkType :: String -> Name
mkType = Ident . capitalize

pQual :: String -> QName
pQual = Qual preludeAlias . Ident

implQual :: String -> QName
implQual = Qual internalModuleAlias . Ident

implType :: String -> Language.Haskell.Exts.Type
implType = TyCon . implQual

intL :: Integral a => a -> Exp
intL n | n >= 0 = Lit $ Int $ fromIntegral n
intL n = NegApp $ intL $ abs n

parenIntL :: Integral a => a -> Exp
parenIntL n | n >= 0 = intL n
parenIntL n = Paren $ intL n

floatL :: Real a => a -> Exp
floatL n | n >= 0 = Lit $ Frac $ toRational n
floatL n = NegApp $ floatL $ abs n

importTemplate :: ImportDecl
importTemplate = ImportDecl
  { importLoc = noLoc, importModule = undefined,
    importQualified = True, importSrc = False, importSafe = False,
    importPkg = Nothing, importAs = Nothing, importSpecs = Nothing
  }

importInternalModule :: ImportDecl
importInternalModule = importTemplate
  { importModule = internalModuleName,
    importAs = Just internalModuleAlias
  }

importPrelude :: ImportDecl
importPrelude = importTemplate
  { importModule = ModuleName "Prelude",
    importAs = Just preludeAlias
  }

mkModuleName :: QualifiedName -> String -> ModuleName
mkModuleName ns typename = ModuleName $ intercalate "." $ map capitalize $ ns ++ [typename]

typeParamConstraint :: QName -> TypeParam -> Asst
typeParamConstraint className t = ClassA className [TyVar $ mkVar $ paramName t]

wildcardFunc :: String -> Exp -> Decl
wildcardFunc f rhs = FunBind [Match noLoc (Ident f) [PWildCard] Nothing (UnGuardedRhs rhs) noBinds]

makeType :: Bool -> Name -> [TypeParam] -> Language.Haskell.Exts.Type
makeType _ typeName [] = TyCon $ UnQual typeName
makeType needParen typeName params
  | needParen = TyParen typeDecl
  | otherwise = typeDecl
  where
  typeDecl = foldl1 TyApp $ (TyCon $ UnQual typeName) : map (TyVar . mkVar . paramName) params

hsType :: String -> MappingContext -> Language.Bond.Syntax.Types.Type -> Language.Haskell.Exts.Type
hsType _ _ BT_Int8 = implType "Int8"
hsType _ _ BT_Int16 = implType "Int16"
hsType _ _ BT_Int32 = implType "Int32"
hsType _ _ BT_Int64 = implType "Int64"
hsType _ _ BT_UInt8 = implType "Word8"
hsType _ _ BT_UInt16 = implType "Word16"
hsType _ _ BT_UInt32 = implType "Word32"
hsType _ _ BT_UInt64 = implType "Word64"
hsType _ _ BT_Float = implType "Float"
hsType _ _ BT_Double = implType "Double"
hsType _ _ BT_Bool = implType "Bool"
hsType _ _ BT_String = implType "Utf8"
hsType _ _ BT_WString = implType "Utf16"
hsType _ _ BT_MetaName = error "BT_MetaName not implemented"
hsType _ _ BT_MetaFullName = error "BT_MetaFullName not implemented"
hsType _ _ BT_Blob = implType "Blob"
hsType _ _ (BT_IntTypeArg _) = error "BT_IntTypeArg not implemented"
hsType s c (BT_Maybe type_) = TyApp (implType "Maybe") (hsType s c type_)
hsType s c (BT_Nullable type_) = TyApp (implType "Maybe") (hsType s c type_)
hsType s c (BT_List element) = TyList $ hsType s c element
hsType s c (BT_Vector element) = TyApp (implType "Vector") (hsType s c element)
hsType s c (BT_Set element) = TyApp (implType s) (hsType s c element)
hsType s c (BT_Map key value) = TyApp (TyApp (implType "Map") (hsType s c key)) (hsType s c value)
hsType s c (BT_Bonded type_) = TyApp (implType "Bonded") (hsType s c type_)
hsType _ _ (BT_TypeParam type_) = TyVar $ mkVar $ paramName type_
hsType _ _ (BT_UserDefined Alias{} _) = error "BT_UserDefined Alias"
hsType s c (BT_UserDefined decl params) = foldl1 TyApp $ declType : map (hsType s c) params
    where
    declType = let ns = getDeclNamespace c decl
                   typename = declName decl
                in TyCon $ Qual (mkModuleName ns typename) (mkType typename)

proxyOf :: Language.Haskell.Exts.Type -> Exp
proxyOf = ExpTypeSig noLoc (Con $ implQual "Proxy") . TyApp (TyCon $ implQual "Proxy")

makeDeclName :: Declaration -> String
makeDeclName decl = overrideName (declName decl) (declAttributes decl)

makeFieldName :: Field -> String
makeFieldName f = overrideName (fieldName f) (fieldAttributes f)

overrideName :: String -> [Attribute] -> String
overrideName def attrs = maybe def attrValue $ find (\a -> attrName a == ["HaskellName"]) attrs

-- overrides for bond functions I can't use because of opaque TypeMapping

getNamespace :: MappingContext -> QualifiedName
getNamespace c = resolveNamespace c (namespaces c)

getQualifiedName :: MappingContext -> QualifiedName -> String
getQualifiedName _ = intercalate "."

getDeclNamespace :: MappingContext -> Declaration -> QualifiedName
getDeclNamespace c = resolveNamespace c . declNamespaces

getDeclTypeName :: MappingContext -> Declaration -> String
getDeclTypeName c = getQualifiedName c . declQualifiedName c

resolveNamespace :: MappingContext -> [Namespace] -> QualifiedName
resolveNamespace c ns =
    maybe namespaceName toNamespace $ find ((namespaceName ==) . fromNamespace) (namespaceMapping c)
    where
    namespaceName = nsName . fromJust $ neutralNamespace <|> fallbackNamespace
    neutralNamespace = find (isNothing . nsLanguage) ns
    fallbackNamespace = Just $ last ns

declQualifiedName :: MappingContext -> Declaration -> QualifiedName
declQualifiedName c decl = getDeclNamespace c decl ++ [declName decl]

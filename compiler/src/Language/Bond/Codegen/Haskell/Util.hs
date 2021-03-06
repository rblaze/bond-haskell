module Language.Bond.Codegen.Haskell.Util where

import Data.Char
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Syntax.Types
import Language.Haskell.Exts hiding (Namespace)
import Language.Haskell.Exts.SrcLoc (noLoc)
import Data.List
import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Builder

data CodegenOpts = CodegenOpts
    { setType :: String
    , deriveEq :: Bool
    , deriveGeneric :: Bool
    , deriveNFData :: Bool
    , deriveShow :: Bool
    }

unique :: Ord a => [a] -> [a]
unique = map head . group . sort

fromBuilder :: Builder -> String
fromBuilder = unpack . toLazyText

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
    { importLoc = noLoc
    , importModule = undefined
    , importQualified = True
    , importSrc = False
    , importSafe = False
    , importPkg = Nothing
    , importAs = Nothing
    , importSpecs = Nothing
    }

importInternalModule :: ImportDecl
importInternalModule = importTemplate
    { importModule = internalModuleName
    , importAs = Just internalModuleAlias
    }

importPrelude :: ImportDecl
importPrelude = importTemplate
    { importModule = ModuleName "Prelude"
    , importAs = Just preludeAlias
    }

importGenerics :: ImportDecl
importGenerics = importTemplate
    { importModule = ModuleName "GHC.Generics"
    , importAs = Just preludeAlias
    }

mkModuleName :: QualifiedName -> String -> ModuleName
mkModuleName ns typename = ModuleName $ intercalate "." $ map capitalize $ ns ++ [typename]

typeParamConstraint :: QName -> TypeParam -> Asst
typeParamConstraint className t = ClassA className [TyVar $ mkVar $ paramName t]

wildcardMatch :: String -> Exp -> Match
wildcardMatch f rhs = Match noLoc (Ident f) [PWildCard] Nothing (UnGuardedRhs rhs) noBinds

wildcardFunc :: String -> Exp -> Decl
wildcardFunc f rhs = FunBind [wildcardMatch f rhs]

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
hsType s c (BT_UserDefined decl params) = foldl1 TyApp $ declType : map (hsType s c) params
    where
    declType = let ns = getDeclNamespace c decl
                   typename = declName decl
                in TyCon $ Qual (mkModuleName ns typename) (mkType typename)

getTypeModules :: Language.Haskell.Exts.Type -> [ModuleName]
getTypeModules (TyCon (Qual moduleName _)) = [moduleName]
getTypeModules (TyApp t1 t2) = getTypeModules t1 ++ getTypeModules t2
getTypeModules (TyList t) = getTypeModules t
getTypeModules _ = []

proxyOf :: Language.Haskell.Exts.Type -> Exp
proxyOf = ExpTypeSig noLoc (Con $ implQual "Proxy") . TyApp (TyCon $ implQual "Proxy")

makeDeclName :: Declaration -> String
makeDeclName decl@Alias{} = declName decl
makeDeclName decl = overrideName (declName decl) (declAttributes decl)

makeFieldName :: Field -> String
makeFieldName f = overrideName (fieldName f) (fieldAttributes f)

overrideName :: String -> [Attribute] -> String
overrideName def attrs = maybe def attrValue $ find (\a -> attrName a == ["HaskellName"]) attrs

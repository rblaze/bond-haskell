module Language.Bond.Codegen.Haskell.SchemaDecl (
    getSchema,
    structNameAndType
  ) where

import Language.Bond.Codegen.Haskell.Util

import Language.Bond.Codegen.TypeMapping (MappingContext(..))
import Language.Bond.Syntax.Types

import Data.Maybe
import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc (noLoc)

makeFieldType :: String -> MappingContext -> Field -> Exp
makeFieldType settype ctx field
    | BT_Bool <- fieldType field = reuseDefault "FieldBool"
    | BT_Int8 <- fieldType field = reuseDefault "FieldInt8"
    | BT_Int16 <- fieldType field = reuseDefault "FieldInt16"
    | BT_Int32 <- fieldType field = reuseDefault "FieldInt32"
    | BT_Int64 <- fieldType field = reuseDefault "FieldInt64"
    | BT_UInt8 <- fieldType field = reuseDefault "FieldUInt8"
    | BT_UInt16 <- fieldType field = reuseDefault "FieldUInt16"
    | BT_UInt32 <- fieldType field = reuseDefault "FieldUInt32"
    | BT_UInt64 <- fieldType field = reuseDefault "FieldUInt64"
    | BT_Float <- fieldType field = reuseDefault "FieldFloat"
    | BT_Double <- fieldType field = reuseDefault "FieldDouble"
    | BT_String <- fieldType field = reuseDefault "FieldString"
    | BT_WString <- fieldType field = reuseDefault "FieldWString"
    | BT_UserDefined Enum{} _ <- fieldType field =
        App (Con $ implQual "FieldInt32") $ Paren $
            App (Con $ implQual "DefaultValue") $ Paren $
                App (Var $ pQual "fromIntegral") $ Paren $
                    App (Var $ pQual "fromEnum") $ Paren $
                        App (Var $ UnQual $ mkVar $ makeFieldName field) (Var $ implQual "defaultValue")
    | BT_Maybe t <- fieldType field =
        App (Var $ implQual "elementToDefNothingFieldType") $ Paren $
            App (Var $ implQual "getElementType") $ Paren $
                proxyOf $ hsType settype ctx t
    | t <- fieldType field = 
        App (Var $ implQual "elementToFieldType") $ Paren $
            App (Var $ implQual "getElementType") $ Paren $
                proxyOf $ hsType settype ctx t
    where
    reuseDefault con = App (Con $ implQual con) $ Paren $
        App (Con $ implQual "DefaultValue") $ Paren $
            App (Var $ UnQual $ mkVar $ makeFieldName field) (Var $ implQual "defaultValue")

getSchema :: CodegenOpts -> MappingContext -> Declaration -> InstDecl
getSchema opts ctx decl = InsDecl $ simpleFun noLoc (Ident "getSchema") (Ident "type'proxy") $
                RecConstr (implQual "StructSchema")
                    [ FieldUpdate (implQual "structTag") $
                        App (Var $ implQual "typeRep") (Var $ unqual "type'proxy")
                    , FieldUpdate (implQual "structName") $
                        App (Var $ implQual "getName") (Var $ unqual "type'proxy")
                    , FieldUpdate (implQual "structQualifiedName") $
                        App (Var $ implQual "getQualifiedName") (Var $ unqual "type'proxy")
                    , FieldUpdate (implQual "structAttrs") $
                        App (Var $ implQual "makeMap") (List $ map makeAttr (declAttributes decl))
                    , FieldUpdate (implQual "structBase") $
                        case structBase decl of
                            Nothing -> Con (pQual "Nothing")
                            Just base -> App (Con $ pQual "Just") $ Paren $
                                App (Var $ implQual "getSchema") (Paren $ proxyOf $ hsType (setType opts) ctx base)
                    , FieldUpdate (implQual "structFields") $
                        App (Var $ implQual "makeMap") (List $ map makeFieldInfo (structFields decl))
                    , FieldUpdate (implQual "structOrdinalsRequiredOnWrite") $
                        App (Var $ implQual "fromOrdinalList") (List $ filterOrdinals (/= Optional) (structFields decl))
                    , FieldUpdate (implQual "structOrdinalsRequiredOnRead") $
                        App (Var $ implQual "fromOrdinalList") (List $ filterOrdinals (== Required) (structFields decl))
                    ]
    where
    filterOrdinals f = mapMaybe (\ field ->
        if f (fieldModifier field)
            then Just $ App (Con $ implQual "Ordinal") (intL $ fieldOrdinal field)
            else Nothing)
    makeFieldInfo field = Tuple Boxed
        [ App (Con $ implQual "Ordinal") (intL $ fieldOrdinal field)
        , RecConstr (implQual "FieldSchema")
            [ FieldUpdate (implQual "fieldName") $ strE $ fieldName field
            , FieldUpdate (implQual "fieldAttrs") $
                App (Var $ implQual "makeMap") (List $ map makeAttr (fieldAttributes field))
            , FieldUpdate (implQual "fieldModifier") $ Con $ implQual $
                case fieldModifier field of
                    Optional -> "FieldOptional"
                    Required -> "FieldRequired"
                    RequiredOptional -> "FieldRequiredOptional"
            , FieldUpdate (implQual "fieldType") $ makeFieldType (setType opts) ctx field
            ]
        ]
    makeAttr a = Tuple Boxed
        [ strE $ getQualifiedName ctx $ attrName a
        , strE $ attrValue a 
        ]


structNameAndType :: MappingContext -> Declaration -> [InstDecl]
structNameAndType ctx decl =
    [ InsDecl $ wildcardFunc "getName" $ nameFunc $ declName decl
    , InsDecl $ wildcardFunc "getQualifiedName" $ nameFunc $ getDeclTypeName ctx{namespaceMapping = []} decl
    , InsDecl $ simpleFun noLoc (Ident "getElementType") (Ident "type'proxy") $
            App (Con $ implQual "ElementStruct") (Paren $ App (Var $ implQual "getSchema") (Var $ unqual "type'proxy"))
    ]
    where
    paramProxies = map (Paren . proxyOf . TyVar . mkVar . paramName) (declParams decl)
    nameFunc ownName = 
        if null (declParams decl)
            then strE ownName
            else appFun (Var $ implQual "makeGenericName")
                    [ strE ownName
                    , List $ map (App (Var $ implQual "getQualifiedName")) paramProxies
                    ]

{-# LANGUAGE NamedFieldPuns #-}

module Language.Bond.Codegen.Haskell.SchemaDecl (
    importSchema,
    sQual,
    typeDefGenDecl
  ) where

import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping (MappingContext)
import Language.Bond.Codegen.Haskell.Util

import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc (noLoc)

schemaModuleAlias :: ModuleName
schemaModuleAlias = ModuleName "S'"

importSchema :: ImportDecl
importSchema = importTemplate
  { importModule = ModuleName "Data.Bond.Schema",
    importAs = Just schemaModuleAlias
  }

sQual :: String -> QName
sQual = Qual schemaModuleAlias . Ident

defUpdate :: [FieldUpdate] -> Exp
defUpdate = RecUpdate (Var $ implQual "defaultValue")

fieldTypeVar :: String -> Name
fieldTypeVar fname = Ident ("type'" ++ fname)

makeAttr :: MappingContext -> Attribute -> Exp
makeAttr ctx a = Tuple Boxed [
    stringL $ getQualifiedName ctx $ attrName a,
    stringL $ attrValue a
  ]

convertDefault :: MappingContext -> Field -> Default -> Exp
convertDefault ctx field defval = case defval of
    DefaultBool b -> updateRec (sQual "uint_value") (intL $ fromEnum b)
    DefaultInteger v ->
        case v of
            _ | fieldType field `elem` [BT_Int8, BT_Int16, BT_Int32, BT_Int64] -> updateRec (sQual "int_value") (intL v)
              | fieldType field `elem` [BT_Float, BT_Double] -> updateRec (sQual "double_value") (intL v)
              | otherwise -> updateRec (sQual "uint_value") (intL v)
    DefaultFloat v -> updateRec (sQual "double_value") (floatL v)
    DefaultString v ->
        if fieldType field == BT_String
            then updateRec (sQual "string_value") $
                    App (Var $ implQual "fromString") (stringL v)
            else updateRec (sQual "wstring_value") $
                    App (Var $ implQual "fromString") (stringL v)
    DefaultEnum v -> let BT_UserDefined decl [] = fieldType field
                         ns = getDeclNamespace ctx decl
                         typename = makeDeclName decl
                      in updateRec (sQual "int_value") $
                            App (Var $ pQual "fromIntegral") $
                            App (Var $ pQual "fromEnum") (Var $ Qual (mkModuleName ns typename) (mkVar v))
    DefaultNothing -> updateRec (sQual "nothing") (Con $ pQual "True")
    where
    updateRec fname fval = defUpdate [FieldUpdate fname fval]

makeFieldDef :: MappingContext -> Field -> Exp
makeFieldDef ctx field = App (App (App (Var $ sQual "makeFieldDef")
    (Paren $ App (App (App
        (case fieldDefault field of
            Nothing -> Var $ sQual "makeFieldMeta"
            Just defval -> App (Var $ sQual "makeFieldMetaWithDef") (convertDefault ctx field defval)
        )
        (stringL $ fieldName field))
        (List $ map (makeAttr ctx) $ fieldAttributes field))
        (Con $ sQual $ case fieldModifier field of
                Optional -> "optional"
                Required -> "required"
                RequiredOptional -> "requiredOptional")))
    (intL $ fieldOrdinal field))
    (Var $ UnQual $ fieldTypeVar (fieldName field))

typeDefGenDecl :: String -> MappingContext -> Declaration -> Decl
typeDefGenDecl setType ctx decl@Struct{} = InstDecl noLoc Nothing []
    (map (typeParamConstraint $ sQual "TypeDefGen") (declParams decl))
    (sQual "TypeDefGen")
    [makeType True typeName (declParams decl)]
    [InsDecl $ simpleFun noLoc (Ident "getTypeDef") proxyVar $
        App (App (Var $ sQual "withStruct") (Var $ UnQual proxyVar)) $ Paren $ Do $
        -- get base typedef
        (case structBase decl of
            Just t -> (mkFindType baseTypeVar (hsType setType ctx t) :)
            Nothing -> id
        )
        -- get field typedefs
        (map (\f -> mkFindType (fieldTypeVar $ fieldName f) (hsUnwrappedType $ fieldType f)) (structFields decl) ++
        -- create struct
        [Qualifier $ App (Var $ pQual "return") $
            Tuple Boxed [
                App (App (App (Var $ sQual "makeStructMeta")
                    (Paren $ App (Var $ sQual "getTypeName") (Var $ UnQual proxyVar)))
                    (Paren $ App (Var $ sQual "getQualifiedTypeName") (Var $ UnQual proxyVar)))
                    (List $ map (makeAttr ctx) $ declAttributes decl),
                case structBase decl of
                    Nothing -> Con $ pQual "Nothing"
                    Just _ -> App (Con $ pQual "Just") (Var $ UnQual baseTypeVar),
                List $ map (makeFieldDef ctx) (structFields decl)
            ]
        ]),
     InsDecl $ wildcardFunc "getTypeName" $
        if null (declParams decl)
            then stringL $ declName decl
            else App (App (Var $ sQual "makeGenericTypeName") (stringL $ declName decl))
                    (List $ map (App (Var $ sQual "getQualifiedTypeName")) paramProxies),
     InsDecl $ wildcardFunc "getQualifiedTypeName" $
        if null (declParams decl)
            then stringL $ getDeclTypeName ctx decl
            else App (App (Var $ sQual "makeGenericTypeName") (stringL $ getDeclTypeName ctx decl))
                    (List $ map (App (Var $ sQual "getQualifiedTypeName")) paramProxies)
    ]
    where
    typeName = mkType (makeDeclName decl)
    proxyVar = Ident "type''proxy"
    baseTypeVar = Ident "type''base"
    hsUnwrappedType (BT_Maybe t) = hsType setType ctx t
    hsUnwrappedType t = hsType setType ctx t
    mkFindType varname typ =
        Generator noLoc (PVar varname) $
            App (Var $ sQual "findTypeDef") $ Paren $
                ExpTypeSig noLoc (Con $ implQual "Proxy") (TyApp (TyCon $ implQual "Proxy") typ)
    paramProxies = map (Paren . ExpTypeSig noLoc (Con $ implQual "Proxy") . TyApp (TyCon $ implQual "Proxy") . TyVar . mkVar . paramName) (declParams decl)

typeDefGenDecl _ ctx decl@Enum{} = InstDecl noLoc Nothing [] []
    (sQual "TypeDefGen")
    [TyCon $ UnQual $ mkType $ makeDeclName decl]
    [InsDecl $ wildcardFunc "getTypeDef" $ App (Var $ sQual "getTypeDef") $
        ExpTypeSig noLoc (Con $ implQual "Proxy") (TyApp (TyCon $ implQual "Proxy") (implType "Int32")),
     InsDecl $ wildcardFunc "getTypeName" $ stringL (declName decl),
     InsDecl $ wildcardFunc "getQualifiedTypeName" $ stringL (getDeclTypeName ctx decl)
    ]

typeDefGenDecl _ _ _ = error "typeDefGenDecl called for invalid type"

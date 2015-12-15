{-# LANGUAGE NamedFieldPuns, PatternGuards, RecordWildCards #-}
module Language.Bond.Codegen.Haskell.StructDecl (
        structDecl,
        structHsBootDecl
    ) where

import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping (MappingContext)
import Language.Bond.Codegen.Haskell.SchemaDecl
import Language.Bond.Codegen.Haskell.Util

import Data.Maybe
import Language.Haskell.Exts
import Language.Haskell.Exts.SrcLoc (noLoc)

baseStructField :: Name
baseStructField = Ident "base'"

getTypeModules :: Language.Haskell.Exts.Type -> [ModuleName]
getTypeModules (TyCon (Qual moduleName _)) = [moduleName]
getTypeModules (TyApp t1 t2) = getTypeModules t1 ++ getTypeModules t2
getTypeModules (TyList t) = getTypeModules t
getTypeModules _ = []

defaultFieldValue :: MappingContext -> Language.Bond.Syntax.Types.Field -> FieldUpdate
defaultFieldValue ctx Field{fieldName, fieldType, fieldDefault}
    = FieldUpdate (UnQual $ mkVar fieldName) (defValue fieldDefault)
    where
    defValue Nothing = Var $ implQual "defaultValue"
    defValue (Just (DefaultBool v)) = Con $ pQual $ show v
    defValue (Just (DefaultInteger v)) = intL v
    defValue (Just (DefaultFloat v)) = floatL v
    defValue (Just (DefaultString v))
        = App (Var $ implQual "fromString") (Lit $ String v)
    defValue (Just (DefaultEnum v))
        = let BT_UserDefined decl [] = fieldType
              ns = getDeclNamespace ctx decl
              typename = declName decl
           in Var $ Qual (mkModuleName ns typename) (mkVar v)
    defValue (Just DefaultNothing) = Con $ pQual "Nothing"

getUntagged :: Name -> Declaration -> InstDecl
getUntagged cname decl = InsDecl $
    patBind noLoc (PVar $ Ident "bondStructGetUntagged") code
    where
    baseVar = Ident "base'struct"
    fieldsGet = map fieldFunc (structFields decl)
    fieldFunc f | fieldDefault f == Just DefaultNothing = Var $ implQual "bondGetDefNothing"
                | BT_Nullable _ <- fieldType f = Var $ implQual "bondGetNullable"
                | otherwise = Var $ implQual "bondGet"
    code | isNothing (structBase decl) = foldl (\a b -> InfixApp a (QVarOp $ implQual "ap") b)
                (App (Var $ pQual "return") (Con $ UnQual cname)) fieldsGet
         | otherwise = Do [
                Generator noLoc (PVar baseVar) (Var $ implQual "bondGetBaseStruct"),
                Qualifier $ foldl (\a b -> InfixApp a (QVarOp $ implQual "ap") b)
                        (App (Var $ pQual "return") (Paren $ App (Con $ UnQual cname) (Var $ UnQual baseVar))) fieldsGet
            ]

getBase :: Declaration -> InstDecl
getBase decl = InsDecl $ FunBind [Match noLoc (Ident "bondStructGetBase") [PVar self] Nothing (UnGuardedRhs code) noBinds]
    where
    self = Ident "self'"
    base = Ident "base'val"
    code | isNothing (structBase decl) = App (Var $ pQual "return") (Var $ UnQual self)
         | otherwise = Do [
                Generator noLoc (PVar base) (Var $ implQual "bondGetBaseStruct"),
                Qualifier $ App (Var $ pQual "return") $ RecUpdate (Var $ UnQual self) [
                    FieldUpdate (UnQual baseStructField) (Var $ UnQual base)
                ]
            ]

getField :: Declaration -> InstDecl
getField decl = InsDecl $ FunBind $ map fieldFunc (structFields decl) ++ [defaultFunc]
    where
    self = Ident "self'"
    val = Ident "field'val"
    defaultFunc = Match noLoc (Ident "bondStructGetField") [PWildCard, PWildCard] Nothing
            (UnGuardedRhs $ App (Var $ pQual "error") (stringL "unknown field ordinal")) noBinds
    fieldFunc f = Match noLoc (Ident "bondStructGetField")
            [PParen $ PApp (implQual "Ordinal") [PLit Signless $ Int $ fromIntegral $ fieldOrdinal f], PVar self]
            Nothing
            (UnGuardedRhs $ Do [
                Generator noLoc (PVar val) (Var $ getFunc f),
                Qualifier $ App (Var $ pQual "return") $ RecUpdate (Var $ UnQual self) [
                    FieldUpdate (UnQual $ mkVar $ fieldName f) (Var $ UnQual val)
                ]
            ]) noBinds
    getFunc f | fieldDefault f == Just DefaultNothing = implQual "bondGetDefNothing"
              | otherwise = implQual "bondGet"

structPut :: Declaration -> InstDecl
structPut decl = InsDecl $ FunBind [Match noLoc (Ident "bondStructPut") [selfPVar] Nothing (UnGuardedRhs code) noBinds]
    where
    self = Ident "self'"
    selfPVar | isNothing (structBase decl) && null (structFields decl) = PWildCard
             | otherwise = PVar self
    code | isNothing (structBase decl) && null (structFields decl) = Var $ UnQual $ Ident "return"
         | otherwise = Do $ map Qualifier (baseCode ++ fieldsCode)
    baseCode | isNothing (structBase decl) = []
             | otherwise = [
                    App (Var $ implQual "bondPutBaseStruct") $ Paren $ App (Var $ UnQual baseStructField) (Var $ UnQual self)
                ]
    fieldsCode = map putField (structFields decl)
    putField f = App
        (App (Var $ putFunc f) $ Paren $ App (Con $ implQual "Ordinal") (intL $ fieldOrdinal f))
        (Paren $ App (Var $ UnQual $ mkVar $ fieldName f) (Var $ UnQual self))
    putFunc f | fieldDefault f == Just DefaultNothing = implQual "bondPutDefNothingField"
              | otherwise = implQual "bondPutField"

structDecl :: CodegenMode -> String -> MappingContext -> ModuleName -> Declaration -> Maybe Module
structDecl mode setType ctx moduleName decl@Struct{..} = Just source
    where
    source = Module noLoc moduleName
        [LanguagePragma noLoc
            [Ident "ScopedTypeVariables", Ident "DeriveDataTypeable"]
        ]
        Nothing
        (Just [EThingAll $ UnQual typeName])
        imports
        [dataDecl, defaultDecl, wiretypeDecl, schemableDecl, bondSerializableDecl,
         bondStructDecl, typeDefGenDecl setType ctx decl
        ]

    imports | mode == SchemaDef = importInternalModule : importPrelude : importSchema{importSrc = True} : map (\ m -> importTemplate{importModule = m}) fieldModules
            | otherwise = importInternalModule : importPrelude : importSchema : map (\ m -> importTemplate{importModule = m}) fieldModules

    typeName = mkType declName
    typeParams = map (\TypeParam{paramName} -> UnkindedVar $ mkVar paramName) declParams
    fieldModules = unique $ filter (/= moduleName) $ filter (/= internalModuleAlias)
                    $ concatMap (getTypeModules . snd) fields
    mkField Field{fieldName, fieldType} = ([mkVar fieldName], hsType setType ctx fieldType)
    ownFields = map mkField structFields
    fields | Just base <- structBase = ([baseStructField], hsType setType ctx base) : ownFields
           | otherwise = ownFields
    dataDecl = DataDecl noLoc DataType [] typeName typeParams
              [QualConDecl noLoc [] [] (RecDecl typeName fields)]
              [(pQual "Show", []), (implQual "Typeable", [])]

    ownFieldDefaults = map (defaultFieldValue ctx) structFields
    fieldDefaults | isNothing structBase = ownFieldDefaults
                  | otherwise = FieldUpdate (UnQual baseStructField) (Var $ implQual "defaultValue") : ownFieldDefaults
    defaultDecl = InstDecl noLoc Nothing []
        (map (typeParamConstraint $ implQual "Default") declParams)
        (implQual "Default")
        [makeType True typeName declParams]
        [InsDecl $
            patBind noLoc (PVar $ Ident "defaultValue") $
                RecConstr (UnQual typeName) fieldDefaults
        ]
    wiretypeDecl = InstDecl noLoc Nothing [] [] (implQual "WireType")
        [makeType True typeName declParams]
        [InsDecl $
            FunBind
                [Match noLoc (Ident "getWireType") [PWildCard] Nothing
                    (UnGuardedRhs (Con $ implQual "bT_STRUCT")) noBinds
                ]
        ]
    bondSerializableDecl = InstDecl noLoc Nothing []
        (map (typeParamConstraint $ implQual "Default") declParams ++
            map (typeParamConstraint $ implQual "BondSerializable") declParams)
        (implQual "BondSerializable")
        [makeType True typeName declParams]
        [InsDecl $
            patBind noLoc (PVar $ Ident "bondGet") $
                Var (implQual "bondGetStruct"),
         InsDecl $
            patBind noLoc (PVar $ Ident "bondPut") $
                Var (implQual "bondPutStruct")
        ]
    bondStructDecl = InstDecl noLoc Nothing []
        (map (typeParamConstraint $ implQual "Default") declParams ++
            map (typeParamConstraint $ implQual "BondSerializable") declParams)
        (implQual "BondStruct")
        [makeType True typeName declParams]
        [structPut decl,
         getUntagged typeName decl,
         getBase decl,
         getField decl
        ]
    schemableDecl = InstDecl noLoc Nothing []
        (map (typeParamConstraint $ sQual "TypeDefGen") declParams)
        (sQual "Schemable")
        [makeType True typeName declParams]
        []

structDecl _ _ _ _ _ = error "structDecl called for invalid type"

structHsBootDecl :: CodegenMode -> String -> MappingContext -> ModuleName -> Declaration -> Maybe Module
structHsBootDecl mode setType ctx moduleName Struct{..} = if mode == SchemaDef then Just hsboot else Nothing
    where
    hsboot = Module noLoc moduleName [] Nothing Nothing
        (importInternalModule{importSrc = True} : map (\ m -> importTemplate{importModule = m, importSrc = True}) fieldModules)
        [
            DataDecl noLoc DataType [] typeName typeParams [QualConDecl noLoc [] [] (RecDecl typeName fields)] [],
            InstDecl noLoc Nothing []
                (map (typeParamConstraint $ implQual "Default") declParams)
                (implQual "Default")
                [makeType True typeName declParams] []
        ]

    typeName = mkType declName
    typeParams = map (\TypeParam{paramName} -> UnkindedVar $ mkVar paramName) declParams
    fieldModules = unique $ filter (/= moduleName) $ filter (/= internalModuleAlias)
                    $ concatMap (getTypeModules . snd) fields
    mkField Field{fieldName, fieldType} = ([mkVar fieldName], hsType setType ctx fieldType)
    ownFields = map mkField structFields
    fields | Just base <- structBase = ([baseStructField], hsType setType ctx base) : ownFields
           | otherwise = ownFields

structHsBootDecl _ _ _ _ _ = error "structDecl called for invalid type"

module Language.Bond.Codegen.Haskell.EnumDecl (
        enumDecl,
        enumHsBootDecl
    ) where

import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping
import Language.Bond.Codegen.Haskell.SchemaDecl
import Language.Bond.Codegen.Haskell.Util
import Language.Haskell.Exts hiding (mode)
import Language.Haskell.Exts.SrcLoc (noLoc)

enumDecl :: CodegenMode -> MappingContext -> ModuleName -> Declaration -> Maybe Module
enumDecl mode ctx moduleName decl@Enum{} = Just source
    where
    source = Module noLoc moduleName
        [LanguagePragma noLoc
            [Ident "GeneralizedNewtypeDeriving", Ident "DeriveDataTypeable"]
        ]
        Nothing
        Nothing
        imports
        (dataDecl : serializableDecl : typeDefGenDecl (error "do not use") ctx decl : typeSig : values)
    imports | mode == SchemaDef = [importInternalModule, importPrelude, importSchema{importSrc = True}]
            | otherwise = [importInternalModule, importPrelude, importSchema]
    typeName = mkType $ makeDeclName decl
    typeCon = TyCon (UnQual typeName)
    dataDecl = DataDecl noLoc NewType [] typeName []
        [QualConDecl noLoc [] [] (ConDecl typeName [implType "Int32"])]
        [(pQual "Show", []), (pQual "Eq", []), (pQual "Ord", []), (pQual "Enum", []),
         (implQual "Hashable", []),
         (implQual "WireType", []), (implQual "Default", []), (implQual "Typeable", [])
        ]
    serializableDecl = InstDecl noLoc Nothing [] [] (implQual "Serializable")
        [typeCon]
        [InsDecl $
            FunBind
                [Match noLoc (Ident "bondPut")
                    [PParen (PApp (UnQual typeName) [PVar $ Ident "v'"])]
                    Nothing
                    (UnGuardedRhs $
                        App (Var $ implQual "bondPut") (Var $ unqual "v'"))
                    noBinds],
         InsDecl $
            PatBind noLoc (PVar $ Ident "bondGet")
                (UnGuardedRhs $
                    appFun (Var $ pQual "fmap") [Con $ UnQual typeName, Var $ implQual "bondGet"])
                noBinds
        ]
    typeSig = TypeSig noLoc (map (mkVar . constantName) (enumConstants decl)) typeCon
    values = makeValue 0 (enumConstants decl)
    makeValue _ [] = []
    makeValue _ (Constant{constantName = cname, constantValue = Just i} : xs)
        = mkConst cname i : makeValue (i + 1) xs
    makeValue i (Constant{constantName = cname} : xs)
        = mkConst cname i : makeValue (i + 1) xs
    mkConst constName val
        = patBind noLoc (PVar $ mkVar constName) $ App (Con $ UnQual typeName) (parenIntL val)

enumDecl _ _ _ _ = error "enumDecl called for invalid type"

enumHsBootDecl :: CodegenMode -> MappingContext -> ModuleName -> Declaration -> Maybe Module
enumHsBootDecl mode _ moduleName decl@Enum{} = if mode == SchemaDef then Just hsboot else Nothing
    where
    hsboot = Module noLoc moduleName [] Nothing Nothing [importInternalModule{importSrc = True}] [
                DataDecl noLoc NewType [] typeName [] [QualConDecl noLoc [] [] (ConDecl typeName [implType "Int32"])] [],
                typeSig
              ]
    typeName = mkType $ makeDeclName decl
    typeCon = TyCon (UnQual typeName)
    typeSig = TypeSig noLoc (map (mkVar . constantName) (enumConstants decl)) typeCon

enumHsBootDecl _ _ _ _ = error "enumDecl called for invalid type"

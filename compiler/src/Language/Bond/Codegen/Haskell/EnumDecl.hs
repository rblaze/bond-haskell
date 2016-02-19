module Language.Bond.Codegen.Haskell.EnumDecl (
        enumDecl,
        enumHsBootDecl
    ) where

import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping (MappingContext(..))
import Language.Bond.Codegen.Haskell.Util
import Language.Haskell.Exts hiding (mode)
import Language.Haskell.Exts.SrcLoc (noLoc)

enumDecl :: CodegenOpts -> MappingContext -> ModuleName -> Declaration -> Maybe Module
enumDecl _ ctx moduleName decl@Enum{} = Just source
    where
    source = Module noLoc moduleName
        [ LanguagePragma noLoc
            [ Ident "GeneralizedNewtypeDeriving"
            , Ident "DeriveDataTypeable"
            , Ident "OverloadedStrings"
            ]
        ]
        Nothing
        Nothing
        [importInternalModule, importPrelude]
        (dataDecl : bondTypeDecl : typeSig : values)
    typeName = mkType $ makeDeclName decl
    typeCon = TyCon (UnQual typeName)
    dataDecl = DataDecl noLoc NewType [] typeName []
        [ QualConDecl noLoc [] [] (ConDecl typeName [implType "Int32"]) ]
        [ (pQual "Show", []), (pQual "Eq", []), (pQual "Ord", []), (pQual "Enum", [])
        , (implQual "Hashable", [])
        , (implQual "WireType", []), (implQual "Default", []), (implQual "Typeable", [])
        ]
    bondTypeDecl = InstDecl noLoc Nothing [] [] (implQual "BondType")
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
                noBinds,
         InsDecl $ wildcardFunc "getName" $ strE (declName decl),
         InsDecl $ wildcardFunc "getQualifiedName" $ strE (getDeclTypeName ctx{namespaceMapping = []} decl),
         InsDecl $ wildcardFunc "getElementType" $ Con (implQual "ElementInt32")
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

enumHsBootDecl :: CodegenOpts -> MappingContext -> ModuleName -> Declaration -> Maybe Module
enumHsBootDecl _ _ moduleName decl@Enum{} = Just hsboot
    where
    hsboot = Module noLoc moduleName [] Nothing Nothing [importInternalModule{importSrc = True}, importPrelude] [
                DataDecl noLoc NewType [] typeName [] [QualConDecl noLoc [] [] (ConDecl typeName [implType "Int32"])] [],
                showInstance,
                eqInstance,
                typeSig
              ]
    typeName = mkType $ makeDeclName decl
    typeCon = TyCon (UnQual typeName)
    typeSig = TypeSig noLoc (map (mkVar . constantName) (enumConstants decl)) typeCon
    showInstance = InstDecl noLoc Nothing [] [] (pQual "Show") [typeCon] []
    eqInstance = InstDecl noLoc Nothing [] [] (pQual "Eq") [typeCon] []

enumHsBootDecl _ _ _ _ = error "enumDecl called for invalid type"

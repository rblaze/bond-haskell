module Language.Bond.Codegen.Haskell.EnumDecl (
        enumDecl,
        enumHsBootDecl
    ) where

import Data.Int
import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping
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
        (dataDecl : bondTypeDecl : bondEnumDecl : typeSig : values)
    typeName = mkType $ makeDeclName decl
    typeCon = TyCon (UnQual typeName)
    dataDecl = DataDecl noLoc NewType [] typeName []
        [ QualConDecl noLoc [] [] (ConDecl typeName [implType "Int32"]) ]
        [ (pQual "Show", []), (pQual "Eq", []), (implQual "NFData", []), (pQual "Ord", []), (pQual "Enum", [])
        , (implQual "Hashable", []), (implQual "Default", []), (implQual "Typeable", [])
        ]
    bondTypeDecl = InstDecl noLoc Nothing [] [] (implQual "BondType")
        [ typeCon ]
        [ InsDecl $
            FunBind
                [Match noLoc (Ident "bondPut")
                    [PParen (PApp (UnQual typeName) [PVar $ Ident "v'"])]
                    Nothing
                    (UnGuardedRhs $
                        App (Var $ implQual "bondPut") (Var $ unqual "v'"))
                    noBinds]
        , InsDecl $
            PatBind noLoc (PVar $ Ident "bondGet")
                (UnGuardedRhs $
                    appFun (Var $ pQual "fmap") [Con $ UnQual typeName, Var $ implQual "bondGet"])
                noBinds
        , InsDecl $ wildcardFunc "getName" $ strE (declName decl)
        , InsDecl $ wildcardFunc "getQualifiedName" $ strE $ fromBuilder $ getDeclTypeName ctx{namespaceMapping = []} decl
        , InsDecl $ wildcardFunc "getElementType" $ Con (implQual "ElementInt32")
        ]
    bondEnumDecl = InstDecl noLoc Nothing [] [] (implQual "BondEnum")
        [ typeCon ]
        [ InsDecl $ FunBind $ map makeToNameMatch consts ++ [wildcardMatch "toName" $ Var $ pQual "Nothing"]
        , InsDecl $ FunBind $ map makeFromNameMatch consts ++ [wildcardMatch "fromName" $ Var $ pQual "Nothing"]
        ]
    makeToNameMatch (constName, i) =
        Match noLoc (Ident "toName") [ PParen $ PApp (UnQual typeName) [intP i] ]
            Nothing (UnGuardedRhs $ App (Var $ pQual "Just") (strE constName)) noBinds
    makeFromNameMatch (constName, _) =
        Match noLoc (Ident "fromName") [ strP constName ]
            Nothing (UnGuardedRhs $ App (Var $ pQual "Just") (Var $ UnQual $ mkVar constName)) noBinds
    typeSig = TypeSig noLoc (map (mkVar . constantName) (enumConstants decl)) typeCon
    consts = makeConst 0 (enumConstants decl)
    makeConst _ [] = []
    makeConst _ (Constant{constantName = cname, constantValue = Just i} : xs)
        = (cname, restrictRange i) : makeConst (i + 1) xs
    makeConst i (Constant{constantName = cname} : xs)
        = (cname, restrictRange i) : makeConst (i + 1) xs
    values = map makeValue consts
    makeValue (constName, val) = patBind noLoc (PVar $ mkVar constName) $ App (Con $ UnQual typeName) (parenIntL val)
    restrictRange :: Int -> Integer
    restrictRange i = fromIntegral (fromIntegral i :: Int32)

enumDecl _ _ _ _ = error "enumDecl called for invalid type"

enumHsBootDecl :: CodegenOpts -> MappingContext -> ModuleName -> Declaration -> Maybe Module
enumHsBootDecl _ _ moduleName decl@Enum{} = Just hsboot
    where
    hsboot = Module noLoc moduleName [] Nothing Nothing [importInternalModule{importSrc = True}, importPrelude] [
                DataDecl noLoc NewType [] typeName [] [QualConDecl noLoc [] [] (ConDecl typeName [implType "Int32"])] [],
                showInstance,
                eqInstance,
                nfdataInstance,
                typeSig
              ]
    typeName = mkType $ makeDeclName decl
    typeCon = TyCon (UnQual typeName)
    typeSig = TypeSig noLoc (map (mkVar . constantName) (enumConstants decl)) typeCon
    showInstance = InstDecl noLoc Nothing [] [] (pQual "Show") [typeCon] []
    eqInstance = InstDecl noLoc Nothing [] [] (pQual "Eq") [typeCon] []
    nfdataInstance = InstDecl noLoc Nothing [] [] (implQual "NFData") [typeCon] []

enumHsBootDecl _ _ _ _ = error "enumDecl called for invalid type"

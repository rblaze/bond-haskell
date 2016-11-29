{-# LANGUAGE NamedFieldPuns #-}
module Language.Bond.Codegen.Haskell.ServiceDecl
    ( serviceDecl
    ) where

import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping (MappingContext(..))
import Language.Bond.Codegen.Haskell.Util
import Language.Haskell.Exts as L
import Language.Haskell.Exts.SrcLoc (noLoc)

serviceDecl :: CodegenOpts -> MappingContext -> ModuleName -> Declaration -> Maybe Module
serviceDecl opts ctx moduleName decl@Service{} = Just source
    where
    source = Module noLoc moduleName
        [ LanguagePragma noLoc
            [ Ident "Rank2Types" ]
        ]
        Nothing
        Nothing -- XXX add exports?
        imports
        [proxyDecl, getFuncTypeSig, getFuncDecl]
    monadName = Ident "m'"
    monadType = TyVar monadName
    constrMonadIO = ClassA (implQual "MonadIO") [monadType]
    serviceName = mkType $ makeDeclName decl
    proxyName = let Ident baseName = serviceName in Ident (baseName ++ "Proxy")
    typeParams = UnkindedVar monadName : map (\TypeParam{paramName} -> UnkindedVar $ mkVar paramName) (declParams decl)
    imports = importInternalCommModule : map (\ m -> importTemplate{importModule = m}) fieldModules
    proxyDecl = DataDecl noLoc DataType [] proxyName typeParams
              [QualConDecl noLoc [] [] (RecDecl proxyName methodFields)]
              []
    fieldModules = unique $ filter (/= moduleName) $ filter (/= internalModuleAlias)
                    $ concatMap (getTypeModules . snd) methodFields
    methodFields = map mkMethod (serviceMethods decl)
    mkMethod m@Function{} =
        let resultType = case methodResult m of
                Nothing -> TyCon $ implQual "Void"
                Just t -> hsType (setType opts) ctx t
            funcResultType = TyApp monadType resultType
            funcType = case methodInput m of
                Nothing -> funcResultType
                Just t -> TyFun (hsType (setType opts) ctx t) funcResultType
         in ([mkVar $ makeMethodName m], TyForall Nothing [constrMonadIO] funcType)
    mkMethod m@Event{} =
        let funcType = case methodInput m of
                Nothing -> funcResultType
                Just t -> TyFun (hsType (setType opts) ctx t) funcResultType
            funcResultType = TyApp monadType (TyTuple Boxed [])
         in ([mkVar $ makeMethodName m], TyForall Nothing [constrMonadIO] funcType)
    getFuncName = Ident "get'proxy"
    getFuncTypeSig = TypeSig noLoc [getFuncName] $
        TyForall Nothing
            ( ClassA (implQual "CommClient") [monadType, TyVar $ Ident "c'"]
            : map (\TypeParam{paramName} -> ClassA (implQual "BondStruct") [TyVar $ mkVar paramName]) (declParams decl)
            ) $
            TyFun (TyVar $ Ident "c'") $
                tyApp (TyCon $ UnQual proxyName) $
                monadType : map (\TypeParam{paramName} -> TyVar $ mkVar paramName) (declParams decl)
    clientParam = Ident "client'"
    getFuncDecl = simpleFun noLoc getFuncName clientParam $
        RecConstr (UnQual proxyName) $ map makeClientCall (serviceMethods decl)
    makeClientCall m@Function{methodInput = Nothing} = FieldUpdate (UnQual $ mkVar $ makeMethodName m) $
        appFun (Var $ implQual "talk")
            [ Var $ UnQual clientParam
            , RecConstr (implQual "Void") []
            ]
    makeClientCall m@Function{} = FieldUpdate (UnQual $ mkVar $ makeMethodName m) $
        App (Var $ implQual "talk") (Var $ UnQual clientParam)
    makeClientCall m@Event{} = FieldUpdate (UnQual $ mkVar $ makeMethodName m) $
        App (Var $ implQual "sendEvent") (Var $ UnQual clientParam)

serviceDecl _ _ _ _ = error "serviceDecl called for invalid type"

tyApp :: L.Type -> [L.Type] -> L.Type
tyApp con [] = con
tyApp con (t:ts) = tyApp (TyApp con t) ts

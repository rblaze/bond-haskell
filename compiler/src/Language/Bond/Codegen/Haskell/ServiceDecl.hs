{-# LANGUAGE NamedFieldPuns #-}
module Language.Bond.Codegen.Haskell.ServiceDecl
    ( serviceDecl
    ) where

import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping (MappingContext(..), getDeclTypeName)
import Language.Bond.Codegen.Haskell.Util
import Language.Haskell.Exts as L
import Language.Haskell.Exts.SrcLoc (noLoc)

serviceDecl :: CodegenOpts -> MappingContext -> ModuleName -> Declaration -> Maybe Module
serviceDecl opts ctx moduleName decl@Service{} = Just source
    where
    source = Module noLoc moduleName
        [LanguagePragma noLoc [Ident "OverloadedStrings"]]
        Nothing
        Nothing
        imports
        (concatMap makeTypeSigAndFunc $ serviceMethods decl)

    monadName = Ident "m'"
    monadType = TyVar monadName
    clientName = Ident "c'"
    clientType = TyVar clientName
    makeTypeSigAndFunc m = [makeTypeSig m, makeFunc m]

    makeTypeSig m = TypeSig noLoc [mkVar $ makeMethodName m] $
        TyForall Nothing
            ( ClassA (implQual "CommClient") [monadType, clientType]
            : map (typeParamConstraint $ implQual "BondStruct") (declParams decl)
            )
            (TyFun clientType $ methodType m)
    eventResultType = TyApp monadType (TyTuple Boxed [])
    methodType m@Event{} =
        case methodInput m of
            Nothing -> eventResultType
            Just t -> TyFun (hsType (setType opts) ctx t) eventResultType
    methodType m@Function{} =
        let resultType = case methodResult m of
                Nothing -> TyCon $ implQual "Void"
                Just t -> hsType (setType opts) ctx t
            funcResultType = TyApp monadType resultType
         in case methodInput m of
                Nothing -> funcResultType
                Just t -> TyFun (hsType (setType opts) ctx t) funcResultType

    clientParam = Ident "client'"
    makeFunc m@Function{methodInput = Nothing} =
        simpleFun noLoc (mkVar $ makeMethodName m) clientParam $
            appFun (Var $ implQual "talk")
                [ strE $ fromBuilder $ getDeclTypeName ctx{namespaceMapping = []}  decl
                , strE $ makeMethodName m
                , Var $ UnQual clientParam
                , RecConstr (implQual "Void") []
                ]
    makeFunc m@Function{} = simpleFun noLoc (mkVar $ makeMethodName m) clientParam $
        appFun (Var $ implQual "talk")
            [ strE $ fromBuilder $ getDeclTypeName ctx{namespaceMapping = []}  decl
            , strE $ makeMethodName m
            , Var $ UnQual clientParam
            ]
    makeFunc m@Event{} = simpleFun noLoc (mkVar $ makeMethodName m) clientParam $
        appFun (Var $ implQual "sendEvent")
            [ strE $ fromBuilder $ getDeclTypeName ctx{namespaceMapping = []}  decl
            , strE $ makeMethodName m
            , Var $ UnQual clientParam
            ]

    imports = importInternalCommModule : map (\ m -> importTemplate{importModule = m}) methodModules
    methodModules = unique $ filter (/= moduleName) $ filter (/= internalModuleAlias)
                    $ concatMap (getTypeModules . methodType) (serviceMethods decl)

serviceDecl _ _ _ _ = error "serviceDecl called for invalid type"

{-# LANGUAGE NamedFieldPuns #-}
module Language.Bond.Codegen.Haskell.ServiceDecl
    ( serviceDecl
    ) where

import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping (MappingContext(..))
import Language.Bond.Codegen.Haskell.Util
import Language.Haskell.Exts hiding (mode)
import Language.Haskell.Exts.SrcLoc (noLoc)

serviceDecl :: CodegenOpts -> MappingContext -> ModuleName -> Declaration -> Maybe Module
serviceDecl opts ctx moduleName decl@Service{} = Just source
    where
    source = Module noLoc moduleName
        [ LanguagePragma noLoc
            [ Ident "Rank2Types"
            ]
        ]
        Nothing
        Nothing -- XXX add exports?
        imports
        [proxyDecl]
    monadName = Ident "m'"
    monadType = TyVar monadName
    constrMonadIO = [ClassA (implQual "MonadIO") [monadType]]
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
         in ([mkVar $ makeMethodName m], TyForall Nothing constrMonadIO funcType)
    mkMethod m@Event{} =
        let funcType = case methodInput m of
                Nothing -> funcResultType
                Just t -> TyFun (hsType (setType opts) ctx t) funcResultType
            funcResultType = TyApp monadType (TyTuple Boxed [])
         in ([mkVar $ makeMethodName m], TyForall Nothing constrMonadIO funcType)

serviceDecl _ _ _ _ = error "serviceDecl called for invalid type"

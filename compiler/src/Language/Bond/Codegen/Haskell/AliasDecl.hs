{-# LANGUAGE NamedFieldPuns #-}
module Language.Bond.Codegen.Haskell.AliasDecl (
        aliasDecl
    ) where

import Language.Bond.Syntax.Types
import Language.Bond.Codegen.TypeMapping (MappingContext(..))
import Language.Bond.Codegen.Haskell.Util
import Language.Haskell.Exts hiding (mode)
import Language.Haskell.Exts.SrcLoc (noLoc)

aliasDecl :: CodegenOpts -> MappingContext -> ModuleName -> Declaration -> Maybe Module
aliasDecl opts ctx moduleName decl@Alias{} = Just source
    where
    source = Module noLoc moduleName
        [ ]
        Nothing
        Nothing
        imports
        [synonymDecl]
    typeName = mkType $ makeDeclName decl
    typeParams = map (\TypeParam{paramName} -> UnkindedVar $ mkVar paramName) (declParams decl)
    typeDecl = hsType (setType opts) ctx $ aliasType decl
    imports = map makeImport $ unique $ getTypeModules typeDecl
    makeImport m | m == internalModuleAlias = importInternalModule
                 | otherwise = importTemplate{importModule = m}
    synonymDecl = TypeDecl noLoc typeName typeParams typeDecl

aliasDecl _ _ _ _ = error "aliasDecl called for invalid type"

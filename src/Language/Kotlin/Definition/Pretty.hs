-----------------------------------------------------------------------------
--
-- Module      :  Language.Kotlin.Pretty
-- Copyright   :  (c) Dieter Vekeman 2019
-- License     :  MIT
--
-- Maintainer  :  Dieter Vekeman <dieter.vekeman@gmail.com>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Language.Kotlin.Definition.Pretty (
  renderDeclarationSourceFile
, renderAmbientDeclaration
, renderAmbientClassDeclaration
, renderAmbientInterfaceDeclaration
, renderAmbientNamespaceDeclaration
, renderMaybe
, sepBy
, dot
) where

import Language.Kotlin.Types
import Text.PrettyPrint
import Prelude hiding ( (<>) )
import Data.Char (isUpper)

import Debug.Trace
import Data.Maybe (fromMaybe)

renderDeclarationSourceFile :: String -> [DeclarationElement] -> String
renderDeclarationSourceFile qualifier els =
  render $ declarationSourceFile qualifier els

declarationSourceFile :: String -> [DeclarationElement] -> Doc
declarationSourceFile qualifier elements =
  text "@file:JsQualifier" <> parens (text "\"" <> text qualifier <> text "\"")
    $$ vcat (map declarationElement elements)

declarationElement :: DeclarationElement -> Doc
declarationElement (InterfaceDeclaration _ e i) =
  renderMaybe exported e
  <+> interface i
declarationElement (ExportDeclaration name) =
  exported Exported
  <+> text "="
  <+> text name
declarationElement (AmbientDeclaration _ e a) =
  renderMaybe exported e
  <+> renderAmbientDeclaration a
declarationElement (TypeAliasDeclaration _ e a) =
  renderMaybe exported e
  <+> renderTypeAlias a

renderAmbientClassDeclaration :: Ambient -> Doc
renderAmbientClassDeclaration (AmbientClassDeclaration comment name ps exts imps els) =
  let (staticElements, nonStaticElements) = splitBy isStaticAmbientClassDeclaration els
  in
  renderCommentPlaceholder comment
  $$ text "open external class"
  <+> text name
  <> renderMaybe typeParameters ps
  <+> extendsAndImplementsClause (fromMaybe [] exts) (fromMaybe [] imps)
  <+> char '{'
  $+$ nest 4 (vcat $ map renderAmbientClassBodyElement nonStaticElements)
  $$ nest 4
      (case staticElements of
        [] -> empty
        _ -> text "companion object"
             <+> char '{'
             $+$ nest 4 (vcat $ map renderAmbientStaticClassBodyElement staticElements)
             $$ char '}'
      )
  $$ char '}'
renderAmbientClassDeclaration _ = empty

renderAmbientInterfaceDeclaration :: Ambient -> Doc
renderAmbientInterfaceDeclaration (AmbientInterfaceDeclaration i) = interface i
renderAmbientInterfaceDeclaration _ = empty

renderAmbientNamespaceDeclaration :: Ambient -> Doc
renderAmbientNamespaceDeclaration (AmbientNamespaceDeclaration _ name ds) =
  text "package"
  <+> sepBy dot text name
  $$ text ""
  $$ nest 0 (vcat (map renderAmbientDeclaration ds))
renderAmbientNamespaceDeclaration _ = empty

renderAmbientDeclaration :: Ambient -> Doc
renderAmbientDeclaration (AmbientVariableDeclaration _ name ty) =
  text "var"
  <+> text name
  <> renderMaybe typeAnnotation ty
renderAmbientDeclaration (AmbientFunctionDeclaration comment name plrt) =
  renderCommentPlaceholder comment
  $$ text "external fun"
  <+> text name
  <> parameterListAndReturnType plrt
  <+> text "="
  <+> text "definedExternally"
  $$ text ""
renderAmbientDeclaration ambient@AmbientClassDeclaration {} =
  renderAmbientClassDeclaration ambient
renderAmbientDeclaration ambient@AmbientInterfaceDeclaration {} =
  renderAmbientInterfaceDeclaration ambient
renderAmbientDeclaration (AmbientEnumDeclaration _ mconst name members) =
  renderMaybe (\ConstEnum -> text "const") mconst <+>
  text "enum" <+> text name <+> braces (sepEndBy comma enumMember members)
  where
  enumMember (_name, val) = text name <+> renderMaybe (\n -> char '=' <+> integer n) val
renderAmbientDeclaration (AmbientModuleDeclaration _ name ds) =
  text "module"
  <+> sepBy dot text name
  <+> braces (vcat (map renderAmbientDeclaration ds))
renderAmbientDeclaration ambient@AmbientNamespaceDeclaration {} =
  renderAmbientNamespaceDeclaration ambient
renderAmbientDeclaration (AmbientExternalModuleDeclaration _ name es) =
  text "module"
  <+> stringLiteral name
  <+> char '{'
  $+$ nest 4 (vcat (map renderAmbientExternalModuleElement es))
  $$ char '}'
renderAmbientDeclaration (AmbientTypeAliasDeclaration a) =
  renderTypeAlias a
renderAmbientDeclaration (AmbientImportDeclaration _ name entityName) =
  text "import"
  <+> text name
  <+> char '='
  <+> renderEntityName entityName
renderAmbientDeclaration (AmbientExternalImportDeclaration _ name imp) =
  text "import"
  <+> text name
  <+> char '='
  <+> text "require"
  <+> stringLiteral imp

renderAmbientExternalModuleElement :: AmbientExternalModuleElement -> Doc
renderAmbientExternalModuleElement (AmbientModuleElement a) = renderAmbientDeclaration a
renderAmbientExternalModuleElement (ExportAssignment name) =
  text "export"
  <+> char '='
  <+> text name
  <+> semi

renderAmbientClassBodyElement :: (CommentPlaceholder, AmbientClassBodyElement) -> Doc
renderAmbientClassBodyElement (comment, AmbientConstructorDeclaration ps) =
  renderCommentPlaceholder comment
  $$ text "constructor"
  <+> parameterList ps
  <+> semi
--renderAmbientClassBodyElement (comment, AmbientMemberDeclaration _isOverride p _s prop (Just overloadedName) (Left ty)) =
--  renderCommentPlaceholder comment
--  $$ text "@JsName" <+> parens (doubleQuotes (propertyName prop))
--  $$ renderMaybe publicOrPrivate p
--  <+> text "open var"
--  <+> propertyName overloadedName <> renderMaybe (typeAnnotation_ Nothing) ty <+>
--  text "=" <+> text "definedExternally"
--renderAmbientClassBodyElement (comment, AmbientMemberDeclaration isOverride p _s prop Nothing (Left ty)) =
renderAmbientClassBodyElement (comment, AmbientMemberDeclaration isOverride p _s prop (Left ty)) =
  renderCommentPlaceholder comment $$ renderMaybe publicOrPrivate p <+>
  (if isOverride
     then text "override" <+> text "var"
     else text "open var"
  )
  <+> propertyName prop <> renderMaybe (typeAnnotation_ Nothing) ty
  <+> text "=" <+> text "definedExternally"
--renderAmbientClassBodyElement (comment, AmbientMemberDeclaration _isOverride p _s prop (Just overloadedName) (Right ps))
--  | not (null prop) && isUpper (head prop) = empty
--  | otherwise =
--    renderCommentPlaceholder comment
--    $$ text "@JsName" <+> parens (doubleQuotes (propertyName prop))
--    $$ renderMaybe publicOrPrivate p
--    <+> text "open fun"
--    <+> propertyName overloadedName <> parameterListAndReturnType ps
--    <+> text "=" <+> text "definedExternally"
--renderAmbientClassBodyElement (comment, AmbientMemberDeclaration isOverride p _s prop Nothing (Right ps))
renderAmbientClassBodyElement (comment, AmbientMemberDeclaration isOverride p _s prop (Right ps))
  | not (null prop) && isUpper (head prop) = empty
  | otherwise =
    renderCommentPlaceholder comment
    $$ renderMaybe publicOrPrivate p
    <+> (if isOverride
         then text "override" <+> text "fun"
         else text "open fun"
        )
    <+> propertyName prop <> parameterListAndReturnType ps <+> text "=" <+> text "definedExternally"
renderAmbientClassBodyElement (comment, AmbientIndexSignature i) =
  renderCommentPlaceholder comment
  $$ renderIndexSignature i

renderAmbientStaticClassBodyElement :: (CommentPlaceholder, AmbientClassBodyElement) -> Doc
renderAmbientStaticClassBodyElement (comment, AmbientConstructorDeclaration ps) =
  renderCommentPlaceholder comment
  $$ text "constructor"
  <+> parameterList ps
  <+> semi
--renderAmbientStaticClassBodyElement (comment, AmbientMemberDeclaration _isOverride p _s prop _overloadedName (Left ty)) =
renderAmbientStaticClassBodyElement (comment, AmbientMemberDeclaration _isOverride p _s prop (Left ty)) =
  renderCommentPlaceholder comment $$ renderMaybe publicOrPrivate p <+>
    -- No override here
  text "var" <+>
  propertyName prop <> renderMaybe (typeAnnotation_ Nothing) ty <+>
  text "=" <+> text "definedExternally"
--renderAmbientStaticClassBodyElement (comment, AmbientMemberDeclaration _isOverride p _s prop _overloadedName (Right ps)) =
renderAmbientStaticClassBodyElement (comment, AmbientMemberDeclaration _isOverride p _s prop (Right ps)) =
  renderCommentPlaceholder comment $$ renderMaybe publicOrPrivate p <+>
    -- No override here
  text "fun" <+>
  propertyName prop <> parameterListAndReturnType ps <+> text "=" <+> text "definedExternally"
renderAmbientStaticClassBodyElement (comment, AmbientIndexSignature i) =
  renderCommentPlaceholder comment
  $$ renderIndexSignature i

renderCommentPlaceholder :: CommentPlaceholder -> Doc
renderCommentPlaceholder commentPlaceholder =
  case commentPlaceholder of
    Left _ -> empty -- text "/** (" <> text (show x) <> text "," <+> text (show y) <> text ")*/"
    Right comment ->
         text "/**"
      $$ nest 1 (renderCommentText (commentText comment))
      $$ nest 1 (renderCommentOther (commentOther comment))
      $$ text " */"

renderCommentText :: [String] -> Doc
renderCommentText commentLines =
  let escapedLines = map escapes commentLines
  in
  vcat $ map (\x -> text "*" <+> text x) escapedLines
  where
    escapes :: String -> String
    escapes ('*':xs) = '\\' : '*' : escapes xs
    escapes (x:xs)       = x : escapes xs
    escapes ""           = ""

renderCommentOther :: [(String, String)] -> Doc
renderCommentOther =
  vcat . map (\(x, y) -> text x <+> text y)

renderIndexSignature :: IndexSignature -> Doc
renderIndexSignature (IndexSignature s sn ty) =
  text s
  <> colon
  <+> stringOrNumber sn
  <> typeAnnotation ty

renderTypeAlias :: TypeAlias -> Doc
renderTypeAlias (TypeAlias _ name ty) =
  text "/*"
  $$ text "type"
  <+> text name
  <+> text "="
  <+> type_ Nothing ty
  $$ text "*/"

dot :: Doc
dot = char '.'

sepEndBy :: Doc -> (a -> Doc) -> [a] -> Doc
sepEndBy s f as = hsep $ map (\e -> f e <+> s) as

renderEntityName :: EntityName -> Doc
renderEntityName (EntityName Nothing e) = text e
renderEntityName (EntityName (Just (ModuleName es)) e) = hcat (punctuate dot (map text es)) <> text e

interface :: Interface -> Doc
interface (Interface _ name ps exts ty) =
  text "external interface"
  <+> text name
  <> renderMaybe typeParameters ps
  <+> renderMaybe (extendsClause) exts
  <+> char '{'
  $+$ nest 4 (typeBody ty)
  $$ char '}'

extendsClause :: [TypeRef] -> Doc
extendsClause rs = text ":" <+> classOrInterfaceTypeList rs

_implementsClause :: [TypeRef] -> Doc
_implementsClause rs = text "implements" <+> classOrInterfaceTypeList rs

extendsAndImplementsClause :: [TypeRef] -> [TypeRef] -> Doc
extendsAndImplementsClause exts imps =
  let extAndImp = exts ++ imps
  in case extAndImp of
    [] -> empty
    _ -> text ":" <+> classOrInterfaceTypeList extAndImp

sepBy :: Doc -> (a -> Doc) -> [a] -> Doc
sepBy s f as = hsep $ punctuate s (map f as)

commaSep :: (a -> Doc) -> [a] -> Doc
commaSep = sepBy comma

classOrInterfaceTypeList :: [TypeRef] -> Doc
classOrInterfaceTypeList = commaSep (typeRef)

objectType :: TypeBody -> Doc
objectType = braces . typeBody

typeBody :: TypeBody -> Doc
typeBody (TypeBody ms) = vcat . map (\(_, m) -> typeMember m) $ ms

typeMember :: TypeMember -> Doc
typeMember (MethodSignature name _mOpt plrt) =
  text "fun"
  <+> propertyName name
--  <> renderMaybe optional mOpt
  <+> parameterListAndReturnType plrt
typeMember (PropertySignature name _opt ty) =
  text "var"
  <+> propertyName name
--  <+> renderMaybe optional opt
  <> renderMaybe typeAnnotation ty
typeMember (CallSignature plrt) = parameterListAndReturnType plrt
typeMember (ConstructSignature tyArgs pl ty) =
  text "new"
  <> renderMaybe typeParameters tyArgs
  <> parens (parameterList pl)
  <> renderMaybe typeAnnotation ty
typeMember (TypeIndexSignature i) = renderIndexSignature i

propertyName :: String -> Doc
propertyName = text

parameterAnnotation :: Maybe Optional -> ParameterType -> Doc
parameterAnnotation mOpt (ParameterType ty) =
  typeAnnotation_ mOpt ty
parameterAnnotation mOpt (ParameterSpecialized str) =
  colon <+> stringLiteral str <> renderMaybe optional mOpt

typeAnnotation :: Type -> Doc
typeAnnotation t@(TypeReference (TypeRef (TypeName _ _name) _)) =
  colon <+> type_ Nothing t
typeAnnotation t =
  colon <+> type_ Nothing t

typeAnnotation_ :: Maybe Optional -> Type -> Doc
typeAnnotation_ mOpt t@(TypeReference (TypeRef (TypeName _ _name) _)) =
  colon <+> type_ mOpt t
typeAnnotation_ mOpt t = colon <+> type_ mOpt t

parameterListAndReturnType :: ParameterListAndReturnType -> Doc
parameterListAndReturnType (ParameterListAndReturnType ps pl ty) =
  renderMaybe typeParameters  ps
  <> parens (parameterList pl)
  <> renderMaybe typeAnnotation ty

parameterList :: [Parameter] -> Doc
parameterList = commaSep parameter

optional :: Optional -> Doc
optional _ = char '?'

parameter :: Parameter -> Doc
parameter (RequiredOrOptionalParameter pop name mOpt ty) =
  renderMaybe publicOrPrivate pop
  <+> text (escapeParamName name)
  <> renderMaybe (parameterAnnotation mOpt) ty
  -- <> renderMaybe optional opt
parameter (RestParameter name ty) =
  text "vararg"
  <+> text name
  <> renderMaybe typeAnnotation ty

escapeParamName :: String -> String
escapeParamName "object"   = "`object`"
escapeParamName x          = x

_static :: Static -> Doc
_static _ = text "static"

publicOrPrivate :: PublicOrPrivate -> Doc
publicOrPrivate Public = text "public"
publicOrPrivate Private = text "private"

stringOrNumber :: StringOrNumber -> Doc
stringOrNumber String = text "String"
stringOrNumber Number = text "number"

typeParameters :: [TypeParameter] -> Doc
typeParameters ps = char '<' <> commaSep typeParameter ps <> char '>'

typeParameter :: TypeParameter -> Doc
typeParameter (TypeParameter name ext) =
--  let maybeAnAliasType = mapAlias name
--  in
--  case maybeAnAliasType of
--    Just aliasType -> type_ Nothing aliasType
--    Nothing ->
      text name
      <+> renderMaybe (\t -> text "extends" <+> type_ Nothing t) ext

type_ :: Maybe Optional -> Type -> Doc
type_ mOpt (ArrayType (Predefined _ AnyType)) = text "Any" <> renderMaybe optional mOpt
type_ _ (ArrayType t) = type_ Nothing  t <+> text "[]"
type_ mOpt (Predefined comment p) =
  case comment of
    Left _ -> empty
    Right _ -> text ""
  $+$ renderCommentPlaceholder comment
  $$ predefinedType p <> renderMaybe optional mOpt
type_ mOpt (TypeReference r) =
  typeRef_ mOpt r
--  let maybeAnAliasType = mapAlias name
--  in
--  case maybeAnAliasType of
--    Just aliasType -> type_ Nothing  aliasType
--    Nothing ->
--      typeRef_ mOpt r
type_ mOpt (ObjectType o) = objectType o <> renderMaybe optional mOpt
type_ mOpt (FunctionType ps pl ret) = parens $
--  text "Function" <> text "<" <> text "*" <> text ">"
  renderMaybe typeParameters ps
  <> parens (parameterList pl)
  <+> text "->"
  <+> type_ Nothing  ret
  <> renderMaybe optional mOpt
type_ _ (ConstructorType ps pl ret) = parens $
  text "new"
  <> renderMaybe typeParameters ps
  <> parens (parameterList pl)
  <+> text "->"
  <+> type_ Nothing  ret
type_ _ (UnionType t1 t2) =
  empty
  $$ type_ Nothing  t1
  <+> text "|"
  <+> type_ Nothing  t2
type_ _ (TupleType ts) = brackets $
  commaSep (type_ Nothing) ts
type_ _ (TypeQuery xs) =
  text "typeof"
  <+> sepBy dot text xs
type_ mOpt (DynamicAliasType _) = text "dynamic" <> renderMaybe optional mOpt
--type_ _ (DynamicType Nothing) = text "dynamic"
--type_ _ (DynamicType (Just types)) =
--  text "dynamic"
--  <+> text "/*"
--  <+> hcat (_type <$> types)
--  <+> text "*/"

typeRef :: TypeRef -> Doc
typeRef = typeRef_ Nothing

typeRef_ :: Maybe Optional -> TypeRef -> Doc
typeRef_ mOpt (TypeRef n as) =
  typeName n
  <> renderMaybe (typeArguments) as
  <> renderMaybe optional mOpt

predefinedType :: PredefinedType -> Doc
-- Maybe this is a Kotlin JS bug?
-- Type of 'defaultValue' doesn't match the type of the overridden var-property 'public open var defaultValue: Any defined in ...'
--         ^^^^^^^^^^^^^^
--           Boolean
predefinedType AnyType = text "dynamic"
predefinedType (NumberType Nothing) = text "Number"
predefinedType (NumberType (Just val))= text "Number" <+> text "/*" <+> text val <+> text "*/"
predefinedType BooleanType = text "Boolean"
predefinedType (StringType Nothing) = text "String"
predefinedType (StringType (Just vals)) = text "String" <+> text "/*" <+> sepBy (text " | ") text vals <+> text "*/"
predefinedType VoidType = text "Unit"
predefinedType DynamicType = text "dynamic"

typeName :: TypeName -> Doc
typeName (TypeName Nothing t) = text t
typeName (TypeName (Just (ModuleName ts)) t) = sepBy  dot text ts <+> text t

typeArguments :: [Type] -> Doc
typeArguments ts = char '<' <> commaSep (type_ Nothing) ts <> char '>'

isStaticAmbientClassDeclaration :: (CommentPlaceholder, AmbientClassBodyElement) -> Bool
isStaticAmbientClassDeclaration (_, AmbientConstructorDeclaration _) = False
--isStaticAmbientClassDeclaration (_, AmbientMemberDeclaration _ _ Nothing _ _ _) = False
isStaticAmbientClassDeclaration (_, AmbientMemberDeclaration _ _ Nothing _ _) = False
--isStaticAmbientClassDeclaration (_, AmbientMemberDeclaration _ _ (Just Static) _ _ _) = True
isStaticAmbientClassDeclaration (_, AmbientMemberDeclaration _ _ (Just Static) _ _) = True
isStaticAmbientClassDeclaration (_, AmbientIndexSignature _) = False
--

{--
Given a function, split the list into a tuple
(<xs satisfying the splitter>, <ys not satisfying the splitter>
-}
splitBy :: (a -> Bool) -> [a] -> ([a], [a])
splitBy f xs = (filter f xs, filter (not . f) xs)
--splitBy :: (a -> Bool) -> [a] -> ([a], [a])
--splitBy splitter
--  = foldr
--      (\ x (as, bs) -> if splitter x then (x : as, bs) else (as, x : bs))
--      ([], [])
--

exported :: Exported -> Doc
exported _ = text "export"

renderMaybe :: (a -> Doc) -> Maybe a -> Doc
renderMaybe = maybe empty

stringLiteral :: String -> Doc
stringLiteral = doubleQuotes . text

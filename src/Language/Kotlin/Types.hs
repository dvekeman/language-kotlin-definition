-----------------------------------------------------------------------------
--
-- Module      :  Language.Kotlin.Types
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

{-# LANGUAGE DeriveDataTypeable #-}

module Language.Kotlin.Types where

import qualified Data.Map as M
import Data.Semigroup
import Data.Data (Typeable, Data)

data Comment = Comment
  { commentText     :: [String]
  , commentOther    :: [(String, String)]
  } deriving (Eq, Show, Data, Typeable)

instance Monoid Comment where
  mempty = Comment [] []
  mappend (Comment ts os) (Comment ts' os') = Comment (ts ++ ts') (os ++ os')

instance Semigroup Comment where
  (<>) = mappend

type CommentPlaceholder = Either (Int, Int) Comment
--  deriving (Show, Data, Typeable)
--instance Eq CommentPlaceholder where
--  _ == _ = True

data DeclarationElement
  = InterfaceDeclaration CommentPlaceholder (Maybe Exported) Interface
  | TypeAliasDeclaration CommentPlaceholder (Maybe Exported) TypeAlias
  | ExportDeclaration String
  | AmbientDeclaration CommentPlaceholder (Maybe Exported) Ambient
  deriving (Eq, Show, Data, Typeable)

data Exported = Exported deriving (Eq, Show, Data, Typeable)

data EntityName = EntityName (Maybe ModuleName) String deriving (Eq, Show, Data, Typeable)

data Interface = Interface CommentPlaceholder String (Maybe [TypeParameter]) (Maybe [TypeRef]) TypeBody deriving (Eq, Show, Data, Typeable)

data TypeAlias = TypeAlias CommentPlaceholder String Type deriving (Eq, Show, Data, Typeable)

data Ambient
  = AmbientVariableDeclaration CommentPlaceholder String (Maybe Type)
  | AmbientFunctionDeclaration CommentPlaceholder String ParameterListAndReturnType
  | AmbientClassDeclaration CommentPlaceholder String (Maybe [TypeParameter]) (Maybe [TypeRef]) (Maybe [TypeRef]) [(CommentPlaceholder, AmbientClassBodyElement)]
  | AmbientInterfaceDeclaration Interface
  | AmbientEnumDeclaration CommentPlaceholder (Maybe ConstEnum) String [(String, Maybe Integer)]
  | AmbientTypeAliasDeclaration TypeAlias
  | AmbientModuleDeclaration CommentPlaceholder [String] [Ambient]
  | AmbientNamespaceDeclaration CommentPlaceholder [String] [Ambient]
  | AmbientExternalModuleDeclaration CommentPlaceholder String [AmbientExternalModuleElement]
  | AmbientImportDeclaration CommentPlaceholder String EntityName
  | AmbientExternalImportDeclaration CommentPlaceholder String String
  deriving (Eq, Show, Data, Typeable)

data AmbientExternalModuleElement
  = AmbientModuleElement Ambient
  | ExportAssignment String
  deriving (Eq, Show, Data, Typeable)

data TypeRef = TypeRef TypeName (Maybe [Type]) deriving (Eq, Show, Data, Typeable)

data AmbientClassBodyElement
  = AmbientConstructorDeclaration [Parameter]
--  | AmbientMemberDeclaration {--isOverride:--}Bool (Maybe PublicOrPrivate) (Maybe Static) String {-- overloaded name--}(Maybe String) (Either (Maybe Type) ParameterListAndReturnType)
  | AmbientMemberDeclaration {--isOverride:--}Bool (Maybe PublicOrPrivate) (Maybe Static) String (Either (Maybe Type) ParameterListAndReturnType)
  | AmbientIndexSignature IndexSignature
  deriving (Eq, Show, Data, Typeable)

data Static = Static deriving (Eq, Show, Data, Typeable)

data Optional = Optional deriving (Eq, Show, Data, Typeable)

data ConstEnum = ConstEnum deriving (Eq, Show, Data, Typeable)

data TypeBody = TypeBody [(CommentPlaceholder, TypeMember)] deriving (Eq, Show, Data, Typeable)

data TypeMember
  = PropertySignature String (Maybe Optional) (Maybe Type)
  | CallSignature ParameterListAndReturnType
  | ConstructSignature (Maybe [TypeParameter]) [Parameter] (Maybe Type)
  | TypeIndexSignature IndexSignature
  | MethodSignature String (Maybe Optional) ParameterListAndReturnType
  deriving (Eq, Show, Data, Typeable)

data IndexSignature = IndexSignature String StringOrNumber Type deriving (Eq, Show, Data, Typeable)

data ParameterListAndReturnType = ParameterListAndReturnType (Maybe [TypeParameter]) [Parameter] (Maybe Type) deriving (Eq, Show, Data, Typeable)

data Parameter
  = RequiredOrOptionalParameter (Maybe PublicOrPrivate) String (Maybe Optional) (Maybe ParameterType)
  | RestParameter String (Maybe Type)
  deriving (Eq, Show, Data, Typeable)

data ParameterType
  = ParameterType Type
  | ParameterSpecialized String
  deriving (Eq, Show, Data, Typeable)

data StringOrNumber = String | Number deriving (Eq, Show, Data, Typeable)

data PublicOrPrivate = Public | Private deriving (Eq, Show, Data, Typeable)

data TypeParameter = TypeParameter String (Maybe Type) deriving (Eq, Show, Data, Typeable)

data Type
  = Predefined CommentPlaceholder PredefinedType
  | TypeReference TypeRef
  | ObjectType TypeBody
  | ArrayType Type
  | UnionType Type Type
  | TupleType [Type]
  | TypeQuery [String]
  | FunctionType (Maybe [TypeParameter]) [Parameter] Type
  | ConstructorType (Maybe [TypeParameter]) [Parameter] Type
  | DynamicAliasType (Maybe [Type])
  deriving (Show, Data, Typeable)
instance Eq Type where
  Predefined _ p1 == Predefined _ p2 = p1 == p2
  TypeReference t1 == TypeReference t2 = t1 == t2
  ObjectType _ == ObjectType _ = True
  ArrayType t1 == ArrayType t2 = t1 == t2
  UnionType u1 u2 == UnionType u3 u4 = (u1 == u3 && u2 == u4) || (u1 == u4) && (u2 == u3)
  TupleType t1s == TupleType t2s = t1s == t2s
  TypeQuery q1 == TypeQuery q2 = q1 == q2
  FunctionType mTypeParams1 params1 type1 == FunctionType mTypeParams2 params2 type2 = (mTypeParams1 == mTypeParams2) && (params1 == params2) && (type1 == type2)
  ConstructorType mTypeParams1 params1 type1 == ConstructorType mTypeParams2 params2 type2 = (mTypeParams1 == mTypeParams2) && (params1 == params2) && (type1 == type2)
  DynamicAliasType mTypes1 == DynamicAliasType mTypes2 = mTypes1 == mTypes2
  _ == _ = False

data TypeName = TypeName (Maybe ModuleName) String deriving (Eq, Show, Data, Typeable)

data ModuleName = ModuleName [String] deriving (Eq, Show, Data, Typeable)

data PredefinedType
  = AnyType
  | NumberType (Maybe String)
  | BooleanType
  | StringType (Maybe [String])
  | VoidType
  | DynamicType
  deriving (Show, Data, Typeable)
instance Eq PredefinedType where
  AnyType == AnyType = True
  (NumberType _) == (NumberType _) = True -- Only the type matters
  BooleanType == BooleanType = True
  (StringType _) == (StringType _) = True -- Only the type matters
  VoidType == VoidType = True
  DynamicType == DynamicType = True
  _ == _ = False


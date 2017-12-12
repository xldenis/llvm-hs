module LLVM.AST.Metadata where

import LLVM.Prelude

data MDNode
  = DIExpression
  | DIGlobalVariableExpression
  | DILocation Int Int DILocalScope
  | DIMacroNode
  | DINode DINode
  | MDTuple
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DINode
  = DIEnumerator
  | DIImportedEntity
  | DIObjCProperty
  | DIScope
  | DISubrange
  | DITemplateParameter
  | DIVariable
  | GenericDINode
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DIScope
  = DICompileUnit
  | DIFile
  | DILocalScope
  | DIModule
  | DINamespace
  | DIType
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DILocalScope
  = DILexicalBlockBase
  | DISubprogram
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DIType
  = DIBasicType
  | DICompositeType
  | DIDerivedType
  | DISubroutineType
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DITemplateParameter
  = DITemplateTypeParameter
  | DITemplateValueParameter
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DILexicalBlockBase
  = DILexicalBlock
  | DILexicalBlockFile
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DIVariable
  = DIGlobalVariable
  | DILocalVariable
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

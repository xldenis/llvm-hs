module LLVM.AST.Metadata where

import LLVM.Prelude

data MDNode
  = DIExpression -- nyi
  | DIGlobalVariableExpression -- nyi
  | DILocation Int Int DILocalScope
  | DIMacroNode -- nyi
  | DINode DINode
  | MDTuple -- nyi
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DINode
  = DIEnumerator Int ShortByteString
  | DIImportedEntity Int ShortByteString DIScope DINode {- ? -} Int
  | DIObjCProperty Int Int ShortByteString DIFile ShortByteString ShortByteString DIType
  | DIScope DIScope
  | DISubrange Int Int
  | DITemplateParameter DITemplateParameter
  | DIVariable DIVariable
  | GenericDINode -- idk yet
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DIScope
  = DICompileUnit
  | DIFile DIFile
  | DILocalScope DILocalScope
  | DIModule
  | DINamespace
  | DIType DIType
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DIFile = File ShortByteString ShortByteString ChecksumKind ShortByteString
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data ChecksumKind = None | MD5 | SHA1 | Last
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DILocalScope
  = DILexicalBlockBase DILexicalBlockBase
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
  = DILexicalBlock DIFile DILocalScope Int Int
  | DILexicalBlockFile DIFile DILocalScope Int
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DIVariable
  = DIGlobalVariable
  | DILocalVariable
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

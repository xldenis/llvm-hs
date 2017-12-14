module LLVM.AST.Metadata where

import LLVM.Prelude
import LLVM.AST.Name

data MDNode
  = DIExpression -- nyi
  | DIGlobalVariableExpression -- nyi
  | DILocation Word32 Word32 DILocalScope
  | DIMacroNode -- nyi
  | DINode DINode
  | MDTuple -- nyi
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DINode
  = DIEnumerator Word32 Name
  | DIImportedEntity Word32 Name DIScope DINode {- ? -} Word32
  | DIObjCProperty Word32 Word32 Name DIFile Name Name DIType
  | DIScope DIScope
  | DISubrange Word32 Word32
  | DITemplateParameter DITemplateParameter
  | DIVariable DIVariable
  | GenericDINode -- idk yet
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DIScope
  = DICompileUnit DIFile ShortByteString Bool ShortByteString Word32 ShortByteString Word32 MDNode MDNode MDNode MDNode MDNode Word64
  | DIFile DIFile
  | DILocalScope DILocalScope
  -- | DIModule Name DIScope
  | DINamespace Name DIScope DIFile Word32
  | DIType DIType
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DIFile = File Name Name ChecksumKind Name
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data ChecksumKind = None | MD5 | SHA1 | Last
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DILocalScope
  = DILexicalBlockBase DILexicalBlockBase
  | DISubprogram Name Name DIScope DIFile Word32 DIType Bool DIType Virtuality Word32 DIFlag {- ? -} Bool MDNode MDNode MDNode MDNode MDNode MDNode {- Should be Metada -}
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data Virtuality = MkFake
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DIType
  = DIBasicType Name Word32 Word32 Encoding
  | DICompositeType Name EnumerationType DIFile Word32 Word32 Word32 [DIType] Name
  | DIDerivedType Tag DIType Word32 Word32
  | DISubroutineType [DIType]
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data Tag
  = Member
  | Pointer
  | Reference
  | Typedef
  | Inheritance
  | PtrToMember
  | Const
  | Friend
  | Volatile
  | Restrict
  | Atomic
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data EnumerationType
  = Array
  | Class
  | Enumeration
  | Structure
  | Union
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data Encoding
  = Address
  | Boolean
  | Float
  | Signed
  | SignedChar
  | Unsigned
  | UnsignedChar
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DITemplateParameter
  = DITemplateTypeParameter Name DIType
  | DITemplateValueParameter Name DIType MDNode -- this mdnode sould be a metadata
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DILexicalBlockBase
  = DILexicalBlock DIFile DILocalScope Word32 Word32
  | DILexicalBlockFile DIFile DILocalScope Word32
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DIVariable
  -- | DIGlobalVariable file scope name linkageName line type isLocal isDefinition declaration variable?
  = DIGlobalVariable DIFile DIScope Name Name Word32 DIType Bool Bool MDNode
  | DILocalVariable DIFile DIScope Name Word32 Word32 [DIFlag]
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DIFlag = MkFakeFlag
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

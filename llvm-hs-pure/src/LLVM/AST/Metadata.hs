module LLVM.AST.Metadata where

import LLVM.Prelude
import LLVM.AST.Name

-- | A 'MetadataNodeID' is a number for identifying a metadata node.
-- Note this is different from "named metadata", which are represented with
-- 'LLVM.AST.NamedMetadataDefinition'.
newtype MetadataNodeID = MetadataNodeID Word
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <http://llvm.org/docs/LangRef.html#metadata>
data MetadataNode' op
  = MetadataNode MDNode
  | MetadataTuple [Maybe (Metadata' op)] -- get rid of the inner maybe... [Metadata' op] is already 0 or more nodes... why does each node need to be optional?
  | MetadataNodeReference MetadataNodeID
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | <http://llvm.org/docs/LangRef.html#metadata>
data Metadata' op
  = MDString ShortByteString -- ^ <http://llvm.org/docs/doxygen/html/classllvm_1_1MDNode.html>
  | MDNode (MetadataNode' op) -- ^ <http://llvm.org/docs/doxygen/html/classllvm_1_1MDNode.html>
  | MDValue op -- ^ <http://llvm.org/docs/doxygen/html/classllvm_1_1ValueAsMetadata.html>
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data MDNode
  = DIExpression -- nyi
  | DIGlobalVariableExpression -- nyi
  | DILocation Word32 Word32 DILocalScope
  | DIMacroNode -- nyi
  | DINode DINode
  -- | MDTuple [Maybe Metadata op] -- nyi
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DINode
  -- | https://llvm.org/doxygen/classllvm_1_1DIEnumerator.html
  = DIEnumerator { nodeValue :: Word64, nodeName :: ShortByteString }
  | DIImportedEntity Word32 Name DIScope DINode {- ? -} Word32
  | DIObjCProperty Word32 Word32 Name DIFile Name Name DIType
  | DIScope DIScope
  | DISubrange Word32 Word32
  | DITemplateParameter DITemplateParameter
  | DIVariable DIVariable
  | GenericDINode -- idk yet
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DIScope
-- | https://llvm.org/docs/LangRef.html#dicompileunit
  = DICompileUnit
    { scopeFile :: DIFile
    , scopeProducer :: ShortByteString
    , scopeOptimized :: Bool
    , scopeFlags :: ShortByteString
    , scopeRuntimeVersion :: Word32
    , scopeDebugFileName :: ShortByteString
    , scopeEmissionKind :: Word32
    , scopeEnumTypes :: MDNode
    , scopeRetainedTypes :: MDNode
    , scopeGlobalVariables :: MDNode
    , scopeImportedEntitites :: MDNode
    , scopeMacros :: MDNode
    , scopeDWOId :: Word64
    }
  | DIFile DIFile
  | DILocalScope DILocalScope
  -- | DIModule Name DIScope
  | DINamespace Name DIScope DIFile Word32
  | DIType DIType
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | https://llvm.org/docs/LangRef.html#difile
data DIFile = File
  { filename :: ShortByteString
  , directory :: ShortByteString
  , checksumKind :: ChecksumKind
  , checksum :: ShortByteString
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data ChecksumKind = None | MD5 | SHA1 | Last
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DILocalScope
  = DILexicalBlockBase DILexicalBlockBase
  -- | https://llvm.org/docs/LangRef.html#disubprogram
  | DISubprogram
    { subprogramName :: ShortByteString
    , subprogramLinkageName :: ShortByteString
    , subprogramScope ::  DIScope
    , subprogramFile :: DIFile
    , subprogramLine :: Word32
    , subprogramType :: DIType
    , subprogramDefinition :: Bool
    , subprogramScopeLine :: Word32
    , subprogramContainingType :: DIType
    , subprogramVirtuality :: Virtuality
    , subprogramVirtualityIndex :: Word32
    , subprogramFlags :: DIFlag {- ? -}
    , subprogramOptimized :: Bool
    , subprogramUnit :: MDNode
    , subprogramTemplateParams :: MDNode
    , subprogramDeclaration :: MDNode
    , subprogramVariables :: MDNode
    , subprogramThrownTypes :: MDNode
    } {- Should be Metada -}
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data Virtuality = MkFake
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DIType
  -- | https://llvm.org/docs/LangRef.html#dibasictype
  = DIBasicType
    { typeName :: Name
    , typeSize :: Word32
    , typeAlign :: Word32
    , typeEncoding :: Encoding
    , typeTag :: Word32
    }
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

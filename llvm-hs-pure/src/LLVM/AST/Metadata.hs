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
  = DIExpression { nodeElements :: [Word64] }
  | DIGlobalVariableExpression
    { nodeVariable :: MDNode, nodeExpression :: MDNode }
  | DILocation { locationLine :: Word32, locationColumn :: Word32, locationScope :: DILocalScope }
  | DIMacroNode DIMacroNode -- nyi
  | DINode DINode
  -- | MDTuple [Maybe Metadata op] -- nyi
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DIMacroNode
  -- | <https://llvm.org/doxygen/classllvm_1_1DIMacro.html>
  -- | <https://llvm.org/docs/LangRef.html#dimacro>
  = DIMacro
    { macroInfo :: Word32
    , macroLine :: Word32
    , macroName :: ShortByteString
    , macroValue :: ShortByteString
    }
  -- | <https://llvm.org/doxygen/classllvm_1_1DIMacroFile.html>
  -- | <https://llvm.org/docs/LangRef.html#dimacrofile>
  | DIMacroFile
    { macroInfo :: Word32
    , macroLine :: Word32
    , macroFile :: DIFile
    , macroElemments :: [DIMacroNode]
    }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DINode
  -- | <https://llvm.org/doxygen/classllvm_1_1DIEnumerator.html>
  = DIEnumerator { nodeValue :: Word64, nodeName :: ShortByteString }
  -- | <https://llvm.org/doxygen/classllvm_1_1DIImportedEntity.html>
  | DIImportedEntity
    { nodeTag :: Word32
    , nodeName :: ShortByteString
    , nodeScope :: DIScope
    , nodeEntity :: DINode
    , nodeFile :: DIFile
    , nodeLine :: Word32
    }
  -- | <https://llvm.org/doxygen/classllvm_1_1DIObjCProperty.html>
  | DIObjCProperty
    { nodeLine :: Word32
    , nodeAttributes :: Word32
    , nodeName :: ShortByteString
    , nodeFile :: DIFile
    , nodeGetterName :: Name
    , nodeSetterName :: Name
    , nodeType :: DIType
    }
  | DIScope DIScope
  -- | <https://llvm.org/doxygen/classllvm_1_1DISubrange.html>
  | DISubrange { nodeCount :: Int64, nodeLowerBound :: Int64 }
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
  | DIModule
    { scopeScope :: DIScope
    , scopeName :: ShortByteString
    , scopeConfigurationMacros :: ShortByteString
    , scopeIncludePath :: ShortByteString
    , scopeISysRoot :: ShortByteString
    }
  | DINamespace { scopeName :: ShortByteString, scopeScope :: DIScope, scopeExportSymbols :: Bool }
  | DIType DIType
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | https://llvm.org/docs/LangRef.html#difile
data DIFile = File
  { filename :: ShortByteString
  , directory :: ShortByteString
  , checksum :: ShortByteString
  , checksumKind :: ChecksumKind
  } deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data ChecksumKind = None | MD5 | SHA1
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
    { typeName :: ShortByteString
    , sizeInBits :: Word64
    , alignInBits :: Word32
    , typeEncoding :: Encoding
    , typeTag :: Word16
    }
  -- | <https://llvm.org/docs/LangRef.html#dicompositetype>
  -- | <https://llvm.org/doxygen/classllvm_1_1DICompositeType.html>
  | DICompositeType
    { typeTag :: Word16
    , typeName :: ShortByteString
    , typeFile :: DIFile
    , typeLine :: Word32
    , typeScope :: DIScope
    , typeBaseType :: DIType
    , sizeInBits :: Word64
    , alignInBits :: Word32
    , offsetInBits :: Word64
    , typeFlags :: [DIFlag]
    , typeElements :: [DINode]
    , typeRuntimeLang :: Word32
    , vtableHolder :: DIType
    , typeTemplateParamters :: [DITemplateParameter]
    , typeIdentifier :: ShortByteString
    }
  -- | <https://llvm.org/docs/LangRef.html#diderivedtype>
  -- | <https://llvm.org/doxygen/classllvm_1_1DIDerivedType.html>
  | DIDerivedType
    { typeTag :: Word16
    , typeName :: ShortByteString
    , typeFile :: DIFile
    , typeLine :: Word32
    , typeScope :: DIScope
    , typeBaseType :: DIType
    , sizeInBits :: Word64
    , alignInBits :: Word32
    , offsetInBits :: Word64
    , typeAddressSpace :: Maybe Word32
    , typeFlags :: [DIFlag] -- turns out this is actually a word32 that represents one of the mutually exclusive flags
    } --Tag DIType Word32 Word32
  -- | <https://llvm.org/docs/LangRef.html#disubroutinetype>
  -- | <https://llvm.org/doxygen/classllvm_1_1DISubroutineType.html>
  | DISubroutineType
    { typeFlags :: [DIFlag]
    , typeCC :: Word8
    , typeTypeArray :: [DIType]
    }
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
  = ArrayEnumeration
  | ClassEnumeration
  | EnumerationEnumeration
  | StructureEnumeration
  | UnionEnumeration
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- TODO: Consider dropping this type since LLVM allows for other attribute types.
data Encoding
  = AddressEncoding
  | BooleanEncoding
  | FloatEncoding
  | SignedEncoding
  | SignedCharEncoding
  | UnsignedEncoding
  | UnsignedCharEncoding
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DITemplateParameter
  -- | https://llvm.org/docs/LangRef.html#ditemplatetypeparameter
  = DITemplateTypeParameter { templateParameterName :: ShortByteString, templateParameterType :: DIType }
  -- | https://llvm.org/docs/LangRef.html#ditemplatevalueparameter
  | DITemplateValueParameter
    { templateParameterName :: ShortByteString
    , templateParameterType :: DIType
    , templateParameterValue :: MDNode -- this should actually be a full value but im not threading that in just yet
    , templateParameterTag :: Word32
    }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DILexicalBlockBase
  = DILexicalBlock
    { lexicalBlockBaseFile :: DIFile
    , lexicalBlockBaseScope :: DILocalScope
    , lexicalBlockLine :: Word32
    , lexicalBlockColumn :: Word32
    }
  | DILexicalBlockFile
    { lexicalBlockBaseFile :: DIFile
    , lexicalBlockBaseScope :: DILocalScope
    , lexicalBlockDiscriminator :: Word32
    }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

data DIVariable
  -- | https://llvm.org/docs/LangRef.html#diglobalvariable
  = DIGlobalVariable
    { variableFile :: DIFile
    , variableScope :: DIScope
    , variableName :: ShortByteString
    , variableLinkageName :: Name
    , variableLine :: Word32
    , variableType :: DIType
    , variableLocal :: Bool
    , variableDefinition :: Bool
    , staticDataMemberDeclaration :: MDNode
    , variableAlignInBits :: Word32
    }
  | DILocalVariable
    { variableFile :: DIFile
    , variableScope :: DIScope
    , variableName :: ShortByteString
    , variableLine :: Word32
    , variableArg :: Word32
    , variableFlags :: [DIFlag]
    , variableType :: DIType
    }
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

toFlags _ = [MkFakeFlag]
data DIFlag = MkFakeFlag
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

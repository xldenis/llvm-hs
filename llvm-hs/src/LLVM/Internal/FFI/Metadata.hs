{-#
  LANGUAGE
  ForeignFunctionInterface
  #-}

module LLVM.Internal.FFI.Metadata where

import LLVM.Prelude

import Foreign.Ptr
import Foreign.C

import LLVM.Internal.FFI.Context
import LLVM.Internal.FFI.PtrHierarchy
import LLVM.Internal.FFI.LLVMCTypes

-- foreign import ccall unsafe "LLVM_Hs_IsAMDString" isAMDString ::
--   Ptr Metadata -> IO (Ptr MDString)

-- foreign import ccall unsafe "LLVM_Hs_IsAMDNode" isAMDNode ::
--   Ptr Metadata -> IO (Ptr MDNode)

foreign import ccall unsafe "LLVM_Hs_IsAMDValue" isAMDValue ::
  Ptr Metadata -> IO (Ptr MDValue)

foreign import ccall unsafe "LLVM_Hs_IsAMetadataOperand" isAMetadataOperand ::
  Ptr Value -> IO (Ptr MetadataAsVal)

{- Ideally this would allow for a lookup of the exact subclass rather than having to check each one
   individually. However, I don't know how to access the class "Kinds" that contain the actual Id
   of each individual class.
-}
foreign import ccall unsafe "LLVM_Hs_GetMetadataClassId" getMetadataClassId ::
  Ptr MDNode -> IO (CUInt)

-- foreign import ccall unsafe "LLVM_Hs_DILocationGetLine" getLine ::
--   Ptr DILocation -> IO (CUInt)

-- foreign import ccall unsafe "LLVM_Hs_DILocationGetColumn" getColumn ::
--   Ptr DILocation -> IO (CUInt)

foreign import ccall unsafe "LLVM_HS_DILocationGetScope" getScope ::
  Ptr DIScope -> IO (Ptr DILocalScope)

foreign import ccall unsafe "LLVM_Hs_GetMDValue" getMDValue ::
  Ptr MDValue -> IO (Ptr Value)

foreign import ccall unsafe "LLVM_Hs_GetMetadataOperand" getMetadataOperand ::
  Ptr MetadataAsVal -> IO (Ptr Metadata)

foreign import ccall unsafe "LLVMGetMDKindIDInContext" getMDKindIDInContext' ::
  Ptr Context -> Ptr CChar -> CUInt -> IO MDKindID

getMDKindIDInContext :: Ptr Context -> (Ptr CChar, CUInt) -> IO MDKindID
getMDKindIDInContext ctx (c, n) = getMDKindIDInContext' ctx c n

foreign import ccall unsafe "LLVM_Hs_GetMDKindNames" getMDKindNames ::
  Ptr Context -> Ptr (Ptr CChar) -> Ptr CUInt -> CUInt -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_MDStringInContext" mdStringInContext' ::
  Ptr Context -> CString -> CUInt -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_MDValue" mdValue ::
  Ptr Value -> IO (Ptr MDValue)

foreign import ccall unsafe "LLVM_Hs_MetadataOperand" metadataOperand ::
  Ptr Context -> Ptr Metadata -> IO (Ptr Value)

mdStringInContext :: Ptr Context -> (CString, CUInt) -> IO (Ptr MDString)
mdStringInContext ctx (p, n) = mdStringInContext' ctx p n

foreign import ccall unsafe "LLVM_Hs_GetMDString" getMDString ::
  Ptr MDString -> Ptr CUInt -> IO CString

foreign import ccall unsafe "LLVM_Hs_MDNodeInContext" createMDNodeInContext' ::
  Ptr Context -> Ptr (Ptr Metadata) -> CUInt -> IO (Ptr MDNode)

createMDNodeInContext :: Ptr Context -> (CUInt, Ptr (Ptr Metadata)) -> IO (Ptr MDNode)
createMDNodeInContext ctx (n, vs) = createMDNodeInContext' ctx vs n

foreign import ccall unsafe "LLVM_Hs_CreateTemporaryMDNodeInContext" createTemporaryMDNodeInContext ::
  Ptr Context -> IO (Ptr MDNode)

foreign import ccall unsafe "LLVM_Hs_DestroyTemporaryMDNode" destroyTemporaryMDNode ::
  Ptr MDNode -> IO ()

foreign import ccall unsafe "LLVM_Hs_GetMDNodeNumOperands" getMDNodeNumOperands ::
  Ptr MDNode -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_GetMDNodeOperands" getMDNodeOperands ::
  Ptr MDNode -> Ptr (Ptr Metadata) -> IO ()

foreign import ccall unsafe "LLVM_Hs_GetNamedMetadataName" getNamedMetadataName ::
  Ptr NamedMetadata -> Ptr CUInt -> IO (Ptr CChar)

foreign import ccall unsafe "LLVM_Hs_GetNamedMetadataNumOperands" getNamedMetadataNumOperands ::
  Ptr NamedMetadata -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_GetNamedMetadataOperands" getNamedMetadataOperands ::
  Ptr NamedMetadata -> Ptr (Ptr MDNode) -> IO ()

foreign import ccall unsafe "LLVM_Hs_NamedMetadataAddOperands" namedMetadataAddOperands' ::
  Ptr NamedMetadata -> Ptr (Ptr MDNode) -> CUInt -> IO ()

foreign import ccall unsafe "LLVM_Hs_MetadataReplaceAllUsesWith" metadataReplaceAllUsesWith ::
  Ptr MDNode -> Ptr Metadata -> IO ()

foreign import ccall unsafe "LLVM_Hs_IsAMDString" isAMDString ::
  Ptr Metadata -> IO (Ptr MDString)

-- foreign import ccall unsafe "LLVM_Hs_IsAValueAsMetadata" isAValueAsMetadata ::
--   Ptr MDNode -> IO (Ptr ValueAsMetadata)

-- foreign import ccall unsafe "LLVM_Hs_IsAConstantAsMetadata" isAConstantAsMetadata ::
--   Ptr MDNode -> IO (Ptr ConstantAsMetadata)

-- These are in the .def but not in the doxygen? Maybe unreleased metadata?
-- foreign import ccall unsafe "LLVM_Hs_IsALocalAsMetadata" isALocalAsMetadata ::
--   Ptr MDNode -> IO (Ptr LocalAsMetadata)

-- foreign import ccall unsafe "LLVM_Hs_IsADistinctMDOperandPlaceholder" isADistinctMDOperandPlaceholder ::
--   Ptr MDNode -> IO (Ptr DistinctMDOperandPlaceholder)

foreign import ccall unsafe "LLVM_Hs_IsAMDNode" isAMDNode ::
  Ptr Metadata -> IO (Ptr MDNode)

foreign import ccall unsafe "LLVM_Hs_IsAMDTuple" isAMDTuple ::
  Ptr MDNode -> IO (Ptr MDNode)

foreign import ccall unsafe "LLVM_Hs_IsADILocation" isADILocation ::
  Ptr MDNode -> IO (Ptr MDNode)

foreign import ccall unsafe "LLVM_Hs_IsADIExpression" isADIExpression ::
  Ptr MDNode -> IO (Ptr MDNode)

foreign import ccall unsafe "LLVM_Hs_IsADIGlobalVariableExpression" isADIGlobalVariableExpression ::
  Ptr MDNode -> IO (Ptr MDNode)

foreign import ccall unsafe "LLVM_Hs_IsADINode" isADINode ::
  Ptr MDNode -> IO (Ptr DINode)

foreign import ccall unsafe "LLVM_Hs_IsAGenericDINode" isAGenericDINode ::
  Ptr MDNode -> IO (Ptr DINode)

foreign import ccall unsafe "LLVM_Hs_IsADISubrange" isADISubrange ::
  Ptr MDNode -> IO (Ptr DINode)

foreign import ccall unsafe "LLVM_Hs_IsADIEnumerator" isADIEnumerator ::
  Ptr MDNode -> IO (Ptr DINode)

foreign import ccall unsafe "LLVM_Hs_IsADIScope" isADIScope ::
  Ptr MDNode -> IO (Ptr DIScope)

foreign import ccall unsafe "LLVM_Hs_IsADIType" isADIType ::
  Ptr MDNode -> IO (Ptr DIType)

foreign import ccall unsafe "LLVM_Hs_IsADIBasicType" isADIBasicType ::
  Ptr MDNode -> IO (Ptr DIType)

foreign import ccall unsafe "LLVM_Hs_IsADIDerivedType" isADIDerivedType ::
  Ptr MDNode -> IO (Ptr DIType)

foreign import ccall unsafe "LLVM_Hs_IsADICompositeType" isADICompositeType ::
  Ptr MDNode -> IO (Ptr DIType)

foreign import ccall unsafe "LLVM_Hs_IsADISubroutineType" isADISubroutineType ::
  Ptr MDNode -> IO (Ptr DIType)

foreign import ccall unsafe "LLVM_Hs_IsADIFile" isADIFile ::
  Ptr MDNode -> IO (Ptr DIFile)

foreign import ccall unsafe "LLVM_Hs_IsADICompileUnit" isADICompileUnit ::
  Ptr MDNode -> IO (Ptr DIScope)

foreign import ccall unsafe "LLVM_Hs_IsADILocalScope" isADILocalScope ::
  Ptr MDNode -> IO (Ptr DILocalScope)

foreign import ccall unsafe "LLVM_Hs_IsADISubprogram" isADISubprogram ::
  Ptr MDNode -> IO (Ptr DILocalScope)

foreign import ccall unsafe "LLVM_Hs_IsADILexicalBlockBase" isADILexicalBlockBase ::
  Ptr MDNode -> IO (Ptr DILexicalBlockBase)

foreign import ccall unsafe "LLVM_Hs_IsADILexicalBlock" isADILexicalBlock ::
  Ptr MDNode -> IO (Ptr DILexicalBlockBase)

foreign import ccall unsafe "LLVM_Hs_IsADILexicalBlockFile" isADILexicalBlockFile ::
  Ptr MDNode -> IO (Ptr DILexicalBlockBase)

foreign import ccall unsafe "LLVM_Hs_IsADINamespace" isADINamespace ::
  Ptr MDNode -> IO (Ptr DIScope)

foreign import ccall unsafe "LLVM_Hs_IsADIModule" isADIModule ::
  Ptr MDNode -> IO (Ptr MDNode)

foreign import ccall unsafe "LLVM_Hs_IsADITemplateParameter" isADITemplateParameter ::
  Ptr MDNode -> IO (Ptr DITemplateParameter)

foreign import ccall unsafe "LLVM_Hs_IsADITemplateTypeParameter" isADITemplateTypeParameter ::
  Ptr MDNode -> IO (Ptr DITemplateParameter)

foreign import ccall unsafe "LLVM_Hs_IsADITemplateValueParameter" isADITemplateValueParameter ::
  Ptr MDNode -> IO (Ptr DITemplateParameter)

foreign import ccall unsafe "LLVM_Hs_IsADIVariable" isADIVariable ::
  Ptr MDNode -> IO (Ptr DIVariable)

foreign import ccall unsafe "LLVM_Hs_IsADIGlobalVariable" isADIGlobalVariable ::
  Ptr MDNode -> IO (Ptr DIVariable)

foreign import ccall unsafe "LLVM_Hs_IsADILocalVariable" isADILocalVariable ::
  Ptr MDNode -> IO (Ptr DIVariable)

foreign import ccall unsafe "LLVM_Hs_IsADIObjCProperty" isADIObjCProperty ::
  Ptr MDNode -> IO (Ptr DINode)

foreign import ccall unsafe "LLVM_Hs_IsADIImportedEntity" isADIImportedEntity ::
  Ptr MDNode -> IO (Ptr DINode)

foreign import ccall unsafe "LLVM_Hs_IsADIMacroNode" isADIMacroNode ::
  Ptr MDNode -> IO (Ptr DIMacroNode)

foreign import ccall unsafe "LLVM_Hs_IsADIMacro" isADIMacro ::
  Ptr MDNode -> IO (Ptr DIMacroNode)

foreign import ccall unsafe "LLVM_Hs_IsADIMacroFile" isADIMacroFile ::
  Ptr MDNode -> IO (Ptr DIMacroNode)


namedMetadataAddOperands :: Ptr NamedMetadata -> (CUInt, Ptr (Ptr MDNode)) -> IO ()
namedMetadataAddOperands nm (n, vs) = namedMetadataAddOperands' nm vs n

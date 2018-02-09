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

data DITemplateParameterArray

data DINodeArray

foreign import ccall unsafe "LLVM_Hs_IsAMDString" isAMDString ::
  Ptr Metadata -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_IsAMDNode" isAMDNode ::
  Ptr Metadata -> IO (Ptr MDNode)

foreign import ccall unsafe "LLVM_Hs_IsAMDValue" isAMDValue ::
  Ptr Metadata -> IO (Ptr MDValue)

foreign import ccall unsafe "LLVM_Hs_IsAMetadataOperand" isAMetadataOperand ::
  Ptr Value -> IO (Ptr MetadataAsVal)

foreign import ccall unsafe "LLVM_Hs_IsADILocation" isADILocation ::
  Ptr MDNode -> IO (Ptr DILocation)

{- Ideally this would allow for a lookup of the exact subclass rather than having to check each one
   individually. However, I don't know how to access the class "Kinds" that contain the actual Id
   of each individual class.
-}
foreign import ccall unsafe "LLVM_Hs_GetMetadataClassId" getMetadataClassId ::
  Ptr MDNode -> IO (MDSubclassID)

foreign import ccall unsafe "LLVM_Hs_DILocationGetLine" getLine ::
  Ptr DILocation -> IO (CUInt)

foreign import ccall unsafe "LLVM_Hs_DILocationGetColumn" getColumn ::
  Ptr DILocation -> IO (CUInt)

foreign import ccall unsafe "LLVM_Hs_DILocationGetScope" getScope ::
  Ptr DILocation -> IO (Ptr DILocalScope)

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

namedMetadataAddOperands :: Ptr NamedMetadata -> (CUInt, Ptr (Ptr MDNode)) -> IO ()
namedMetadataAddOperands nm (n, vs) = namedMetadataAddOperands' nm vs n

-- DIEnumerator

foreign import ccall unsafe "LLVM_Hs_Get_DIEnumerator" getDIEnumerator ::
  Ptr Context -> Int64 -> CString -> IO (Ptr DINode)

foreign import ccall unsafe "LLVM_Hs_DIEnumerator_GetValue" getDIEnumeratorValue ::
  Ptr DINode -> IO Int64

foreign import ccall unsafe "LLVM_Hs_DIEnumerator_GetName" getDIEnumeratorName ::
  Ptr DINode -> IO CString


foreign import ccall unsafe "LLVM_Hs_DIFileGetFilename" getFileFilename ::
  Ptr DIFile -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_DIFileGetDirectory" getFileDirectory ::
  Ptr DIFile -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_DIFileGetChecksum" getFileChecksum ::
  Ptr DIFile -> IO (Ptr MDString)

foreign import ccall unsafe "LLVM_Hs_DIFileGetChecksumKind" getFileEnumeratorName ::
  Ptr DIFile -> IO (CUInt)

foreign import ccall unsafe "LLVM_Hs_DIScopeGetName" getScopeName ::
  Ptr DIScope -> Ptr CUInt -> IO CString

foreign import ccall unsafe "LLVM_Hs_DITypeGetName" getTypeName ::
  Ptr DIType -> Ptr CUInt -> IO CString

foreign import ccall unsafe "LLVM_Hs_DITypeGetAlignInBits" getTypeAlignInBits ::
  Ptr DIType -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_DITypeGetSizeInBits" getTypeSizeInBits ::
  Ptr DIType -> IO CULong

foreign import ccall unsafe "LLVM_Hs_DITypeGetOffsetInBits" getTypeOffsetInBits ::
  Ptr DIType -> IO CULong

foreign import ccall unsafe "LLVM_Hs_DIBasicTypeGetEncoding" getBasicTypeEncoding ::
  Ptr DIType -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_DINodeGetTag" getTag ::
  Ptr DINode -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_DITypeGetLine" getTypeLine ::
  Ptr DIType -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_DITypeGetFlags" getTypeFlags ::
  Ptr DIType -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_DICompositeTypeGetElements" getElements ::
  Ptr DIType -> IO (Ptr DINodeArray)

foreign import ccall unsafe "LLVM_Hs_DICompositeTypeGetVTableHolder" getVTableHolder ::
  Ptr DIType -> IO (Ptr DIType)

foreign import ccall unsafe "LLVM_Hs_DICompositeTypeGetBaseType" getCompositeBaseType ::
  Ptr DIType -> IO (Ptr DIType)

foreign import ccall unsafe "LLVM_Hs_DICompositeTypeGetRuntimeLang" getRuntimeLang ::
  Ptr DIType -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_DICompositeTypeGetTemplateParameters" getTemplateParams ::
  Ptr DIType -> IO (Ptr DITemplateParameterArray)

foreign import ccall unsafe "LLVM_Hs_DICompositeTypeGetIdentifier" getIdentifier ::
  Ptr DIType -> Ptr CUInt -> IO CString

foreign import ccall unsafe "LLVM_Hs_DINamespaceGetFile" getNamespaceFile ::
  Ptr DINode -> IO (Ptr DIFile)

foreign import ccall unsafe "LLVM_Hs_DINamespaceGetExportSymbols" getNamespaceExportedSymbols ::
  Ptr DINode -> IO Bool

foreign import ccall unsafe "LLVM_Hs_DIScopeGetScope" getScopeScope ::
  Ptr DIScope -> IO (Ptr DIScope)

foreign import ccall unsafe "LLVM_Hs_DIScopeGetFile" getScopeFile ::
  Ptr DIScope -> IO (Ptr DIFile)

foreign import ccall unsafe "LLVM_Hs_DILexicalBlockBaseGetScope" getLexicalBlockScope ::
  Ptr DILexicalBlockBase -> IO (Ptr DILocalScope)

foreign import ccall unsafe "LLVM_Hs_DILexicalBlockFileGetDiscriminator" getLexicalBlockFileDiscriminator ::
  Ptr DILexicalBlockBase -> IO (CUInt)

foreign import ccall unsafe "LLVM_Hs_DILexicalBlockGetLine" getLexicalBlockLine ::
  Ptr DILexicalBlockBase -> IO (CUInt)

foreign import ccall unsafe "LLVM_Hs_DILexicalBlockGetColumn" getLexicalBlockColumn ::
  Ptr DILexicalBlockBase -> IO (CUInt)

foreign import ccall unsafe "LLVM_Hs_DIDerivedTypeGetBaseType" getDerivedBaseType ::
  Ptr DIType -> IO (Ptr DIType)

foreign import ccall unsafe "LLVM_Hs_DIDerivedTypeGetAddressSpace" getDerivedAddressSpace ::
  Ptr DIType -> Ptr CUInt -> IO LLVMBool

foreign import ccall unsafe "LLVM_Hs_DISubroutineTypeGetCC" getSubroutineCC ::
  Ptr DIType -> IO CUChar

data DITypeRefArray

-- foreign import ccall unsafe "LLVM_Hs_GetDISubroutineTypeArray" getSubroutineTypeArray ::
--   Ptr DIType -> Ptr DITypeRefArray

foreign import ccall unsafe "LLVM_Hs_DISubroutineTypeArrayLength" getSubroutineTypeArraySize ::
  Ptr DIType -> IO CUInt

foreign import ccall unsafe "LLVM_Hs_GetDISubroutineTypeArray" getSubroutineTypeArray ::
  Ptr DIType -> Ptr (Ptr DIType) -> IO ()

foreign import ccall unsafe "LLVM_Hs_Get_DIBasicType" getDIBasicType ::
  Ptr Context -> CUInt -> CString -> Word64 -> Word32 -> CUInt -> IO (Ptr DIType)

foreign import ccall unsafe "LLVM_Hs_Get_DIFile" getDIFile ::
  Ptr Context -> CString -> CString -> CUInt -> CString -> IO (Ptr DIFile)

-- DISubrange
foreign import ccall unsafe "LLVM_Hs_Get_DISubrange" getDISubrange ::
  Ptr Context -> Int64 -> Int64 -> IO (Ptr DINode)

foreign import ccall unsafe "LLVM_Hs_DISubrange_GetCount" getDISubrangeCount ::
  Ptr DINode -> IO Int64

foreign import ccall unsafe "LLVM_Hs_DISubrange_GetLowerBound" getDISubrangeLowerBound ::
  Ptr DINode -> IO Int64

{-# LANGUAGE
  MultiParamTypeClasses,
  NamedFieldPuns,
  OverloadedStrings,
  QuasiQuotes
  #-}
module LLVM.Internal.Operand where

import LLVM.Prelude

import LLVM.Exception

import Control.Monad.Catch
import Control.Monad.State
import Control.Monad.AnyCont
import qualified Data.Map as Map

import Foreign.Ptr
import Foreign.C.Types

import qualified LLVM.Internal.FFI.Constant as FFI
import qualified LLVM.Internal.FFI.InlineAssembly as FFI
import qualified LLVM.Internal.FFI.Metadata as FFI
import qualified LLVM.Internal.FFI.PtrHierarchy as FFI
import qualified LLVM.Internal.FFI.Value as FFI

import LLVM.Internal.Coding
import LLVM.Internal.Constant ()
import LLVM.Internal.Context
import LLVM.Internal.DecodeAST
import LLVM.Internal.EncodeAST
import LLVM.Internal.InlineAssembly ()
import LLVM.Internal.Metadata (getByteStringFromFFI)

import qualified LLVM.AST as A

import LLVM.Internal.FFI.LLVMCTypes (mdSubclassIdP)

instance DecodeM DecodeAST A.Operand (Ptr FFI.Value) where
  decodeM v = do
    c <- liftIO $ FFI.isAConstant v
    if c /= nullPtr
     then
      return A.ConstantOperand `ap` decodeM c
     else
      do m <- liftIO $ FFI.isAMetadataOperand v
         if m /= nullPtr
            then A.MetadataOperand <$> decodeM m
            else A.LocalReference
                   <$> (decodeM =<< liftIO (FFI.typeOf v))
                   <*> getLocalName v

instance DecodeM DecodeAST A.Metadata (Ptr FFI.Metadata) where
  decodeM md = do
    s <- liftIO $ FFI.isAMDString md
    if s /= nullPtr
      then A.MDString <$> decodeM s
      else do
        n <- liftIO $ FFI.isAMDNode md
        if n /= nullPtr
          then A.MDNode <$> decodeM n
          else do v <- liftIO $ FFI.isAMDValue md
                  if v /= nullPtr
                    then A.MDValue <$> decodeM v
                    else fail "Metadata was not one of [MDString, MDValue, MDNode]"

instance DecodeM DecodeAST A.DINode (Ptr FFI.DINode) where
  decodeM diN = do
    sId <- liftIO $ FFI.getMetadataClassId (FFI.upCast diN)
    case sId of
      [mdSubclassIdP|DIEnumerator|] -> do
        value <- liftIO (FFI.getDIEnumeratorValue diN)
        name <- decodeM =<< liftIO (FFI.getDIEnumeratorName diN)
        return $ A.DIEnumerator value name
      [mdSubclassIdP|DIImportedEntity|] -> fail "DIImportedEntity"
      [mdSubclassIdP|DIObjCProperty|]   -> fail "DIObjCProperty"
      [mdSubclassIdP|DISubrange|]       -> do
        count <- liftIO (FFI.getDISubrangeCount diN)
        lowerBound <- liftIO (FFI.getDISubrangeLowerBound diN)
        pure (A.DISubrange count lowerBound)
      [mdSubclassIdP|DIBasicType|]        -> A.DIScope <$> decodeM (castPtr diN :: Ptr FFI.DIScope)
      [mdSubclassIdP|DICompositeType|]    -> A.DIScope <$> decodeM (castPtr diN :: Ptr FFI.DIScope)
      [mdSubclassIdP|DIDerivedType|]      -> A.DIScope <$> decodeM (castPtr diN :: Ptr FFI.DIScope)
      [mdSubclassIdP|DISubroutineType|]   -> A.DIScope <$> decodeM (castPtr diN :: Ptr FFI.DIScope)
      [mdSubclassIdP|DILexicalBlock|]     -> A.DIScope <$> decodeM (castPtr diN :: Ptr FFI.DIScope)
      [mdSubclassIdP|DILexicalBlockFile|] -> A.DIScope <$> decodeM (castPtr diN :: Ptr FFI.DIScope)
      [mdSubclassIdP|DIFile|]             -> A.DIScope <$> decodeM (castPtr diN :: Ptr FFI.DIScope)
      [mdSubclassIdP|DINamespace|]        -> A.DIScope <$> decodeM (castPtr diN :: Ptr FFI.DIScope)
      [mdSubclassIdP|DISubprogram|]       -> A.DIScope <$> decodeM (castPtr diN :: Ptr FFI.DIScope)
      [mdSubclassIdP|DICompileUnit|]      -> A.DIScope <$> decodeM (castPtr diN :: Ptr FFI.DIScope)
      [mdSubclassIdP|DIModule|]           -> A.DIScope <$> decodeM (castPtr diN :: Ptr FFI.DIScope)

      [mdSubclassIdP|DIGlobalVariable|] -> A.DIVariable <$> decodeM (castPtr diN :: Ptr FFI.DIVariable)
      [mdSubclassIdP|DILocalVariable|]  -> A.DIVariable <$> decodeM (castPtr diN :: Ptr FFI.DIVariable)

      [mdSubclassIdP|DITemplateTypeParameter|]  -> A.DITemplateParameter <$> decodeM (castPtr diN :: Ptr FFI.DITemplateParameter)
      [mdSubclassIdP|DITemplateValueParameter|] -> A.DITemplateParameter <$> decodeM (castPtr diN :: Ptr FFI.DITemplateParameter)

      [mdSubclassIdP|DistinctMDOperandPlaceholder|] -> fail "DistinctMDOperandPlaceholder"

      _ -> fail "not a valid DINode subclass id"

instance EncodeM EncodeAST A.DINode (Ptr FFI.DINode) where
  encodeM (A.DISubrange { A.nodeCount, A.nodeLowerBound }) = do
    Context c <- gets encodeStateContext
    liftIO (FFI.getDISubrange c nodeCount nodeLowerBound)
  encodeM (A.DIEnumerator { A.nodeValue, A.nodeName }) = do
    name <- encodeM nodeName
    Context c <- gets encodeStateContext
    liftIO (FFI.getDIEnumerator c nodeValue name)

instance DecodeM DecodeAST A.DIScope (Ptr FFI.DIScope) where
  decodeM p = do
    sId <- liftIO $ FFI.getMetadataClassId (FFI.upCast p)

    case sId of
      [mdSubclassIdP|DINamespace|] -> do
        scope <- decodeM =<< liftIO (FFI.getScopeScope p)
        name  <- getByteStringFromFFI FFI.getScopeName p
        exported <- liftIO $ FFI.getNamespaceExportedSymbols (FFI.upCast p)
        return $ A.DINamespace name scope exported
      [mdSubclassIdP|DIFile|] -> do
        diFile <- decodeM (castPtr p :: Ptr FFI.DIFile)
        return $ A.DIFile diFile
      [mdSubclassIdP|DILexicalBlock|]     -> A.DILocalScope <$> decodeM (castPtr p :: Ptr FFI.DILocalScope)
      [mdSubclassIdP|DILexicalBlockFile|] -> A.DILocalScope <$> decodeM (castPtr p :: Ptr FFI.DILocalScope)
      [mdSubclassIdP|DISubprogram|]       -> A.DILocalScope <$> decodeM (castPtr p :: Ptr FFI.DILocalScope)

      [mdSubclassIdP|DIBasicType|]      -> A.DIType <$> decodeM (castPtr p :: Ptr FFI.DIType)
      [mdSubclassIdP|DICompositeType|]  -> A.DIType <$> decodeM (castPtr p :: Ptr FFI.DIType)
      [mdSubclassIdP|DIDerivedType|]    -> A.DIType <$> decodeM (castPtr p :: Ptr FFI.DIType)
      [mdSubclassIdP|DISubroutineType|] -> A.DIType <$> decodeM (castPtr p :: Ptr FFI.DIType)

      [mdSubclassIdP|DICompileUnit|] -> do
        file <- decodeM =<< liftIO (FFI.getScopeFile p)
        pure A.DICompileUnit
          { A.scopeLanguage = 0
          , A.scopeFile = file
          , A.scopeProducer = ""
          , A.scopeOptimized = False
          , A.scopeFlags = ""
          , A.scopeRuntimeVersion = 0
          , A.scopeDebugFileName = ""
          , A.scopeEmissionKind = 0
          , A.scopeEnumTypes = Nothing
          , A.scopeRetainedTypes = Nothing
          , A.scopeGlobalVariables = Nothing
          , A.scopeImportedEntities = Nothing
          , A.scopeMacros = Nothing
          , A.scopeDWOId = 0
          }
      [mdSubclassIdP|DIModule|]      -> fail "DIModule"
      _ -> fail "Not a valid DIScope subclass ID"

instance DecodeM DecodeAST A.DIFile (Ptr FFI.DIFile) where
  decodeM diF = do
    when (diF == nullPtr) $ error "DIFile is null."
    fname <- decodeM =<< liftIO (FFI.getFileFilename diF)
    dir   <- decodeM =<< liftIO (FFI.getFileDirectory diF)
    cksum <- decodeM =<< liftIO (FFI.getFileChecksum diF)
    csk   <- decodeM =<< liftIO (FFI.getFileEnumeratorName diF)
    return $ A.File fname dir cksum csk

instance EncodeM EncodeAST A.DIFile (Ptr FFI.DIFile) where
  encodeM (A.File {A.filename, A.directory, A.checksum, A.checksumKind}) = do
    filename <- encodeM filename
    directory <- encodeM directory
    checksum <- encodeM checksum
    checksumKind <- encodeM checksumKind
    Context c <- gets encodeStateContext
    liftIO (FFI.getDIFile c filename directory checksumKind checksum)

instance Applicative m => EncodeM m A.Encoding CUInt where
  -- TODO generate this based on LLVM’s HANDLE_DW_ATE macro
  encodeM e = pure $
    case e of
      A.AddressEncoding -> 1
      A.BooleanEncoding -> 2
      A.FloatEncoding -> 4
      A.SignedEncoding -> 5
      A.SignedCharEncoding -> 6
      A.UnsignedEncoding -> 7
      A.UnsignedCharEncoding -> 8

instance MonadThrow m => DecodeM m A.Encoding CUInt where
  decodeM e =
    case e of
      1 -> pure A.AddressEncoding
      2 -> pure A.BooleanEncoding
      4 -> pure A.FloatEncoding
      5 -> pure A.SignedEncoding
      6 -> pure A.SignedCharEncoding
      7 -> pure A.UnsignedEncoding
      8 -> pure A.UnsignedCharEncoding
      _ -> throwM (DecodeException ("Unknown DI encoding: " <> show e))

instance Applicative m => EncodeM m A.ChecksumKind CUInt where
  encodeM k = pure $
    case k of
      A.None -> 0
      A.MD5 -> 1
      A.SHA1 -> 2

instance MonadThrow m => DecodeM m A.ChecksumKind CUInt where
  decodeM k =
    case k of
      0 -> pure A.None
      1 -> pure A.MD5
      2 -> pure A.SHA1
      _ -> throwM (DecodeException ("Unknown ChecksumKind: " <> show k))

instance EncodeM EncodeAST A.DIType (Ptr FFI.DIType) where
  encodeM (A.DIBasicType {A.typeName, A.sizeInBits, A.alignInBits, A.typeEncoding, A.typeTag}) = do
    typeName <- encodeM typeName
    typeEncoding <- encodeM typeEncoding
    Context c <- gets encodeStateContext
    liftIO (FFI.getDIBasicType c (fromIntegral typeTag) typeName sizeInBits alignInBits typeEncoding)

instance DecodeM DecodeAST A.DIType (Ptr FFI.DIType) where
  decodeM diTy = do
    sId <- liftIO $ FFI.getMetadataClassId (FFI.upCast diTy)

    case sId of
      [mdSubclassIdP|DIBasicType|] -> do
        name <- getByteStringFromFFI FFI.getTypeName (castPtr diTy)

        size     <- fmap fromIntegral $ liftIO $ FFI.getTypeSizeInBits diTy
        align    <- fmap fromIntegral $ liftIO $ FFI.getTypeAlignInBits diTy
        encoding <- decodeM =<< liftIO (FFI.getBasicTypeEncoding diTy)
        tag      <- fmap fromIntegral $ liftIO $ FFI.getTag (FFI.upCast diTy)
        return $ A.DIBasicType name size align encoding tag
      [mdSubclassIdP|DICompositeType|]  -> do
        name <- getByteStringFromFFI FFI.getTypeName (castPtr diTy)

        file   <- decodeM =<< liftIO (FFI.getScopeFile (castPtr diTy))
        scope  <- decodeM =<< liftIO (FFI.getScopeScope (castPtr diTy))
        baseTy <- decodeM =<< liftIO (FFI.getCompositeBaseType diTy)

        line     <- fmap fromIntegral $ liftIO $ FFI.getTypeLine diTy
        size     <- fmap fromIntegral $ liftIO $ FFI.getTypeSizeInBits diTy
        align    <- fmap fromIntegral $ liftIO $ FFI.getTypeAlignInBits diTy
        offset   <- fmap fromIntegral $ liftIO $ FFI.getTypeOffsetInBits diTy
        tag      <- fmap fromIntegral $ liftIO $ FFI.getTag (FFI.upCast diTy)

        flags <- fmap fromIntegral $ liftIO $ FFI.getTypeFlags diTy
        els   <- decodeM =<< liftIO (FFI.getElements diTy)

        runLang <- fmap fromIntegral $ liftIO $ FFI.getRuntimeLang diTy
        vtable <-  decodeM =<< liftIO (FFI.getVTableHolder diTy)
        tmplParams <- decodeM =<< liftIO (FFI.getTemplateParams diTy)

        tyIdent <-  getByteStringFromFFI FFI.getIdentifier diTy

        return $ A.DICompositeType
          { A.typeTag = tag
          , A.typeName = name
          , A.typeFile = file
          , A.typeLine = line
          , A.typeScope = scope
          , A.typeBaseType = baseTy
          , A.sizeInBits = size
          , A.alignInBits = align
          , A.offsetInBits = offset
          , A.typeFlags = []
          , A.typeElements = els
          , A.typeRuntimeLang = runLang
          , A.vtableHolder = vtable
          , A.typeTemplateParamters = tmplParams
          , A.typeIdentifier = tyIdent
          }
      [mdSubclassIdP|DIDerivedType|]    -> do
        name <- getByteStringFromFFI FFI.getTypeName (castPtr diTy)

        file   <- decodeM =<< liftIO (FFI.getScopeFile (castPtr diTy))
        scope  <- decodeM =<< liftIO (FFI.getScopeScope (castPtr diTy))
        baseTy <- decodeM =<< liftIO (FFI.getDerivedBaseType diTy)

        line     <- fmap fromIntegral $ liftIO $ FFI.getTypeLine diTy
        size     <- fmap fromIntegral $ liftIO $ FFI.getTypeSizeInBits diTy
        align    <- fmap fromIntegral $ liftIO $ FFI.getTypeAlignInBits diTy
        offset   <- fmap fromIntegral $ liftIO $ FFI.getTypeOffsetInBits diTy
        tag      <- fmap fromIntegral $ liftIO $ FFI.getTag (FFI.upCast diTy)

        flags <- fmap fromIntegral $ liftIO $ FFI.getTypeFlags diTy

        x <- alloca

        isJust <- liftIO $ FFI.getDerivedAddressSpace diTy x
        x' <- peek x
        addressSpace <- decodeM (x', isJust)
        return $ A.DIDerivedType
          { A.typeTag = tag
          , A.typeName = name
          , A.typeFile = file
          , A.typeLine = line
          , A.typeScope = scope
          , A.typeBaseType = baseTy
          , A.sizeInBits = size
          , A.alignInBits = align
          , A.offsetInBits = offset
          , A.typeAddressSpace = addressSpace
          , A.typeFlags = []
          }
      [mdSubclassIdP|DISubroutineType|] -> do
        flags <- fmap fromIntegral $ liftIO $ FFI.getTypeFlags diTy
        cc <-  fmap fromIntegral $ liftIO $ FFI.getSubroutineCC diTy

        n <- liftIO $ FFI.getSubroutineTypeArraySize diTy
        ops <- allocaArray n
        liftIO $ FFI.getSubroutineTypeArray diTy ops
        arr <- decodeM (n, ops)

        return $ A.DISubroutineType
          { A.typeFlags = []
          , A.typeCC = cc
          , A.typeTypeArray = arr
          }

instance DecodeM DecodeAST A.DIVariable (Ptr FFI.DIVariable) where
  decodeM p = do
    sId <- liftIO $ FFI.getMetadataClassId (FFI.upCast p)
    case sId of
      [mdSubclassIdP|DIGlobalVariable|] -> fail "DIGlobalVariable"
      [mdSubclassIdP|DILocalVariable|] -> do
        file <- decodeM =<< liftIO (FFI.getDIVariableFile p)
        scope <- decodeM =<< liftIO (FFI.getDIVariableScope p)
        name <- decodeM =<< liftIO (FFI.getDIVariableName p)
        line <- decodeM =<< liftIO (FFI.getDIVariableLine p)
        type' <- decodeM =<< liftIO (FFI.getDIVariableType p)
        pure A.DILocalVariable
          { A.variableFile = file
          , A.variableScope = scope
          , A.variableName = name
          , A.variableLine = line
          , A.variableArg = 0
          , A.variableFlags = []
          , A.variableType = type'
          }

instance DecodeM DecodeAST A.DITemplateParameter (Ptr FFI.DITemplateParameter) where

instance DecodeM DecodeAST [A.DINode] (Ptr FFI.DINodeArray) where

instance DecodeM DecodeAST [A.DITemplateParameter] (Ptr FFI.DITemplateParameterArray) where

instance DecodeM DecodeAST A.DILocalScope (Ptr FFI.DILocalScope) where
  decodeM ls = do
    sId <- liftIO $ FFI.getMetadataClassId (FFI.upCast ls)
    case sId of
      [mdSubclassIdP|DISubprogram|] -> do
        name  <- getByteStringFromFFI FFI.getScopeName (castPtr ls)
        file  <- decodeM =<< liftIO (FFI.getScopeFile (castPtr ls))
        scope <- decodeM =<< liftIO (FFI.getScopeScope (FFI.upCast ls))
        line <- decodeM =<< liftIO (FFI.getDISubprogramLine (castPtr ls))
        -- virtuality <- decodeM =<< liftIO (FFI.getDISubprogramVirtuality (castPtr ls))
        virtualIndex <- decodeM =<< liftIO (FFI.getDISubprogramVirtualIndex (castPtr ls))
        scopeLine <- decodeM =<< liftIO (FFI.getDISubprogramScopeLine (castPtr ls))
        optimized <- decodeM =<< liftIO (FFI.isOptimized (castPtr ls))
        definition <- decodeM =<< liftIO (FFI.isDefinition (castPtr ls))
        pure $ A.DISubprogram
          { A.subprogramName = name
          , A.subprogramLinkageName = ""
          , A.subprogramScope = scope
          , A.subprogramFile = file
          , A.subprogramLine = line
          , A.subprogramType = Nothing
          , A.subprogramDefinition = definition
          , A.subprogramScopeLine = scopeLine
          , A.subprogramContainingType = Nothing
          , A.subprogramVirtuality = A.Virtuality 0
          , A.subprogramVirtualityIndex = virtualIndex
          , A.subprogramFlags = []
          , A.subprogramOptimized = optimized
          , A.subprogramUnit = Nothing
          , A.subprogramTemplateParams = Nothing
          , A.subprogramDeclaration = Nothing
          , A.subprogramVariables = Nothing
          , A.subprogramThrownTypes = Nothing
          }
      [mdSubclassIdP|DILexicalBlock|] -> do
        file  <- decodeM =<< liftIO (FFI.getScopeFile (castPtr ls))
        scope <- decodeM =<< liftIO (FFI.getLexicalBlockScope (castPtr ls))
        line  <- fmap fromIntegral $ liftIO $ FFI.getLexicalBlockLine (castPtr ls)
        col   <- fmap fromIntegral $ liftIO $ FFI.getLexicalBlockColumn (castPtr ls)

        return . A.DILexicalBlockBase $ A.DILexicalBlock file scope line col
      [mdSubclassIdP|DILexicalBlockFile|] -> do
        file  <- decodeM =<< liftIO (FFI.getScopeFile (castPtr ls))
        scope <- decodeM =<< liftIO (FFI.getLexicalBlockScope (castPtr ls))
        disc  <- fmap fromIntegral $ liftIO $ FFI.getLexicalBlockFileDiscriminator (castPtr ls)

        return .  A.DILexicalBlockBase $ A.DILexicalBlockFile file scope disc
      _ -> fail "Expected DILocalScope pointer"

instance DecodeM DecodeAST A.CallableOperand (Ptr FFI.Value) where
  decodeM v = do
    ia <- liftIO $ FFI.isAInlineAsm v
    if ia /= nullPtr
     then Left <$> decodeM ia
     else Right <$> decodeM v

instance EncodeM EncodeAST A.Operand (Ptr FFI.Value) where
  encodeM (A.ConstantOperand c) = (FFI.upCast :: Ptr FFI.Constant -> Ptr FFI.Value) <$> encodeM c
  encodeM (A.LocalReference t n) = do
    lv <- refer encodeStateLocals n $ do
      lv <- do
        n <- encodeM n
        t <- encodeM t
        v <- liftIO $ FFI.createArgument t n
        return $ ForwardValue v
      modify $ \s -> s { encodeStateLocals = Map.insert n lv $ encodeStateLocals s }
      return lv
    return $ case lv of DefinedValue v -> v; ForwardValue v -> v
  encodeM (A.MetadataOperand md) = do
    md' <- encodeM md
    Context c <- gets encodeStateContext
    liftIO $ FFI.upCast <$> FFI.metadataOperand c md'

instance EncodeM EncodeAST A.Metadata (Ptr FFI.Metadata) where
  encodeM (A.MDString s) = do
    Context c <- gets encodeStateContext
    s <- encodeM s
    FFI.upCast <$> liftIO (FFI.mdStringInContext c s)
  encodeM (A.MDNode mdn) = (FFI.upCast :: Ptr FFI.MDNode -> Ptr FFI.Metadata) <$> encodeM mdn
  encodeM (A.MDValue v) = do
     v <- encodeM v
     FFI.upCast <$> liftIO (FFI.mdValue v)

instance EncodeM EncodeAST A.CallableOperand (Ptr FFI.Value) where
  encodeM (Right o) = encodeM o
  encodeM (Left i) = (FFI.upCast :: Ptr FFI.InlineAsm -> Ptr FFI.Value) <$> encodeM i

instance EncodeM EncodeAST A.MDNode (Ptr FFI.MDNode) where
  encodeM (A.MDTuple ops) = scopeAnyCont $ do
    Context c <- gets encodeStateContext
    ops <- encodeM ops
    liftIO $ FFI.createMDNodeInContext c ops
  encodeM (A.MetadataNodeReference n) = referMDNode n

instance DecodeM DecodeAST [Maybe A.Metadata] (Ptr FFI.MDNode) where
  decodeM p = scopeAnyCont $ do
    n <- liftIO $ FFI.getMDNodeNumOperands p
    ops <- allocaArray n
    liftIO $ FFI.getMDNodeOperands p ops
    decodeM (n, ops)

instance DecodeM DecodeAST A.Operand (Ptr FFI.MDValue) where
  decodeM = decodeM <=< liftIO . FFI.getMDValue

instance DecodeM DecodeAST A.Metadata (Ptr FFI.MetadataAsVal) where
  decodeM = decodeM <=< liftIO . FFI.getMetadataOperand

decodeMDNode :: Ptr FFI.MDNode -> DecodeAST A.MDNode
decodeMDNode p = scopeAnyCont $ do
  sId <- liftIO $ FFI.getMetadataClassId p
  case sId of
      [mdSubclassIdP|DIExpression|] -> decodeDIExpression p
      [mdSubclassIdP|MDTuple|] -> A.MDTuple <$> decodeM p
      [mdSubclassIdP|DIGlobalVariableExpression|] -> fail "DIGlobalVariableExpression"
      [mdSubclassIdP|DILocation|] -> do
        line <- liftIO $ fromIntegral <$> FFI.getLine (castPtr p)
        col  <- liftIO $ fromIntegral <$> FFI.getColumn (castPtr p)
        ptr <-  liftIO $ FFI.getScope (castPtr p)
        scope <- decodeM ptr
        return $ A.DILocation line col scope
      [mdSubclassIdP|DIMacro|] -> fail "DIMacro"
      [mdSubclassIdP|DIMacroFile|] -> fail "DIMacroFile"
      _ -> A.DINode <$> decodeM (castPtr p :: Ptr FFI.DINode)

decodeDIExpression :: Ptr FFI.MDNode -> DecodeAST A.MDNode
decodeDIExpression p = do
  let diExpr = castPtr p
  numElems <- liftIO (FFI.getDIExpressionNumElements diExpr)
  if numElems == 0
    then pure (A.DIExpression [])
    else A.DIExpression <$> traverse (liftIO . FFI.getDIExpressionElement diExpr) [0 .. numElems-1]

instance DecodeM DecodeAST A.MDNode (Ptr FFI.MDNode) where
  decodeM p = scopeAnyCont $ do
    sId <- liftIO $ FFI.getMetadataClassId p
    case sId of
      [mdSubclassIdP|DIExpression|] -> decodeDIExpression p
      _ -> A.MetadataNodeReference <$> getMetadataNodeID p

instance (DecodeM DecodeAST a (Ptr b), FFI.DescendentOf FFI.MDNode b) => DecodeM DecodeAST (A.MDRef a) (Ptr b) where
  decodeM p = scopeAnyCont $
    A.MDRef <$> getMetadataNodeID (FFI.upCast p)

getMetadataDefinitions :: DecodeAST [A.Definition]
getMetadataDefinitions = fix $ \continue -> do
  mdntd <- takeMetadataNodeToDefine
  case mdntd of
    Nothing -> pure []
    Just (mid, p) ->
      (:)
        <$> (A.MetadataNodeDefinition mid <$> decodeMDNode p)
        <*> continue

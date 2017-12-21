{-# LANGUAGE
  MultiParamTypeClasses,
  QuasiQuotes
  #-}
module LLVM.Internal.Operand where

import LLVM.Prelude

import Control.Monad.State
import Control.Monad.AnyCont
import qualified Data.Map as Map

import Foreign.Ptr

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
    if (c /= nullPtr)
     then
      return A.ConstantOperand `ap` decodeM c
     else
      do m <- liftIO $ FFI.isAMetadataOperand v
         if (m /= nullPtr)
            then A.MetadataOperand <$> decodeM m
            else return A.LocalReference
                           `ap` (decodeM =<< (liftIO $ FFI.typeOf v))
                           `ap` getLocalName v

instance DecodeM DecodeAST A.Metadata (Ptr FFI.Metadata) where
  decodeM md = do
    s <- liftIO $ FFI.isAMDString md
    if (s /= nullPtr)
      then A.MDString <$> decodeM s
      else do
        n <- liftIO $ FFI.isAMDNode md
        if (n /= nullPtr)
          then A.MDNode <$> decodeM n
          else do v <- liftIO $ FFI.isAMDValue md
                  if (v /= nullPtr)
                      then A.MDValue <$> decodeM v
                      else fail "Metadata was not one of [MDString, MDValue, MDNode]"

instance DecodeM DecodeAST A.MDNode (Ptr FFI.MDNode) where
  decodeM mdn = do
    sId <- liftIO $ FFI.getMetadataClassId mdn
    case sId of
      [mdSubclassIdP|DILocation|] -> do
        line <- liftIO $ fromIntegral <$> FFI.getLine (castPtr mdn)
        col  <- liftIO $ fromIntegral <$> FFI.getColumn (castPtr mdn)
        ptr <-  liftIO $ FFI.getScope (castPtr mdn)
        scope <- decodeM (ptr)

        return $ A.DILocation line col scope
      [mdSubclassIdP|DIEnumerator|] -> do
        val <- liftIO $ fromIntegral <$> FFI.getEnumeratorValue (castPtr mdn)
        nm  <- decodeM =<< (liftIO $ FFI.getEnumeratorName (castPtr mdn))

        return $ A.DINode $ A.DIEnumerator val nm

      [mdSubclassIdP|DIBasicType|] -> fail "DIBasicType"
      [mdSubclassIdP|DICompileUnit|] -> fail "DICompileUnit"
      [mdSubclassIdP|DICompositeType|] -> fail "DICompositeType"
      [mdSubclassIdP|DIDerivedType|] -> fail "DIDerivedType"
      [mdSubclassIdP|DIExpression|] -> fail "DIExpression"
      [mdSubclassIdP|DIGlobalVariable|] -> fail "DIGlobalVariable"
      [mdSubclassIdP|DIGlobalVariableExpression|] -> fail "DIGlobalVariableExpression"
      [mdSubclassIdP|DIImportedEntity|] -> fail "DIImportedEntity"
      [mdSubclassIdP|DILexicalBlock|] -> A.DINode <$> A.DIScope <$> decodeM (castPtr mdn :: Ptr FFI.DIScope)
      [mdSubclassIdP|DILexicalBlockFile|] -> A.DINode <$> A.DIScope <$> decodeM (castPtr mdn :: Ptr FFI.DIScope)
      [mdSubclassIdP|DILocalVariable|] -> fail "DILocalVariable"
      [mdSubclassIdP|DIMacro|] -> fail "DIMacro"
      [mdSubclassIdP|DIMacroFile|] -> fail "DIMacroFile"
      [mdSubclassIdP|DIModule|] -> fail "DIModule"
      [mdSubclassIdP|DICompileUnit|] -> A.DINode <$> A.DIScope <$> decodeM (castPtr mdn :: Ptr FFI.DIScope)
      [mdSubclassIdP|DIFile|] -> A.DINode <$> A.DIScope <$> decodeM (castPtr mdn :: Ptr FFI.DIScope)
      [mdSubclassIdP|DINamespace|] -> A.DINode <$> A.DIScope <$> decodeM (castPtr mdn :: Ptr FFI.DIScope)
      [mdSubclassIdP|DIObjCProperty|] -> fail "DIObjCProperty"
      [mdSubclassIdP|DistinctMDOperandPlaceholder|] -> fail "DistinctMDOperandPlaceholder"
      [mdSubclassIdP|DISubprogram|] -> A.DINode <$> A.DIScope <$> decodeM (castPtr mdn :: Ptr FFI.DIScope)
      [mdSubclassIdP|DISubrange|] -> fail "DISubrange"
      [mdSubclassIdP|DISubroutineType|] -> fail "DISubroutineType"
      [mdSubclassIdP|DITemplateTypeParameter|] -> fail "DITemplateTypeParameter"
      [mdSubclassIdP|DITemplateValueParameter|] -> fail "DITemplateValueParameter"

      otherwise -> fail "omg"

instance DecodeM DecodeAST A.DIScope (Ptr FFI.DIScope) where
  decodeM p = do
    sId <- liftIO $ FFI.getMetadataClassId (FFI.upCast p)

    case sId of
      [mdSubclassIdP|DINamespace|] -> do
        scope <- decodeM =<< (liftIO $ FFI.getScopeScope p)
        name  <- getByteStringFromFFI FFI.getScopeName p
        exported <- liftIO $ FFI.getNamespaceExportedSymbols (FFI.upCast p)
        return $ A.DINamespace name scope exported
      [mdSubclassIdP|DICompileUnit|] -> fail "DICompileUnit"
      [mdSubclassIdP|DIFile|] -> do
        diFile <- decodeM (castPtr p :: Ptr FFI.DIFile)
        return $ A.DIFile diFile
      [mdSubclassIdP|DILexicalBlock|] -> A.DILocalScope <$> decodeM (castPtr p :: Ptr FFI.DILocalScope)
      [mdSubclassIdP|DILexicalBlockFile|] -> A.DILocalScope <$> decodeM (castPtr p :: Ptr FFI.DILocalScope)
      [mdSubclassIdP|DISubprogram|] -> A.DILocalScope <$> decodeM (castPtr p :: Ptr FFI.DILocalScope)

      otherwise -> fail "DIScope"

instance DecodeM DecodeAST A.DIFile (Ptr FFI.DIFile) where
  decodeM diF = do
    fname <- decodeM =<< (liftIO $ FFI.getFileFilename diF)
    dir   <- decodeM =<< (liftIO $ FFI.getFileDirectory diF)
    cksum <- decodeM =<< (liftIO $ FFI.getFileChecksum diF)
    csk   <-             (liftIO $ FFI.getFileEnumeratorName diF)

    return $ A.File fname dir cksum (toEnum $ fromIntegral csk)

instance DecodeM DecodeAST A.DILocalScope (Ptr FFI.DILocalScope) where
  decodeM ls = do
    sId <- liftIO $ FFI.getMetadataClassId (FFI.upCast ls)
    case sId of
      [mdSubclassIdP|DISubprogram|] -> do
        -- file  <- decodeM =<< (liftIO $ FFI.getScopeFile (castPtr ls))
        scope <- decodeM =<< (liftIO $ FFI.getScopeScope (castPtr ls))
        name  <- getByteStringFromFFI FFI.getScopeName (castPtr ls)
        return $ A.DISubprogram name undefined scope -- file undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined undefined
        fail "DISubprogram"
      [mdSubclassIdP|DILexicalBlock|] -> do
        file  <- decodeM =<< (liftIO $ FFI.getScopeFile (castPtr ls))
        scope <- decodeM =<< (liftIO $ FFI.getLexicalBlockScope (castPtr ls))
        line  <- fmap fromIntegral $ liftIO $ FFI.getLexicalBlockLine (castPtr ls)
        col   <- fmap fromIntegral $ liftIO $ FFI.getLexicalBlockColumn (castPtr ls)

        return . A.DILexicalBlockBase $ A.DILexicalBlock file scope line col
      [mdSubclassIdP|DILexicalBlockFile|] -> do
        file  <- decodeM =<< (liftIO $ FFI.getScopeFile (castPtr ls))
        scope <- decodeM =<< (liftIO $ FFI.getLexicalBlockScope (castPtr ls))
        disc  <- fmap fromIntegral $ liftIO $ FFI.getLexicalBlockFileDiscriminator (castPtr ls)

        return .  A.DILexicalBlockBase $ A.DILexicalBlockFile file scope disc
      otherwise -> fail "Expected DILocalScope pointer"

instance DecodeM DecodeAST A.CallableOperand (Ptr FFI.Value) where
  decodeM v = do
    ia <- liftIO $ FFI.isAInlineAsm v
    if ia /= nullPtr
     then liftM Left (decodeM ia)
     else liftM Right (decodeM v)

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
    liftM FFI.upCast $ liftIO $ FFI.mdStringInContext c s
  encodeM (A.MDNode mdn) = (FFI.upCast :: Ptr FFI.MDNode -> Ptr FFI.Metadata) <$> encodeM mdn
  encodeM (A.MDValue v) = do
     v <- encodeM v
     liftIO $ FFI.upCast <$> FFI.mdValue v

instance EncodeM EncodeAST A.CallableOperand (Ptr FFI.Value) where
  encodeM (Right o) = encodeM o
  encodeM (Left i) = liftM (FFI.upCast :: Ptr FFI.InlineAsm -> Ptr FFI.Value) (encodeM i)

instance EncodeM EncodeAST A.MetadataNode (Ptr FFI.MDNode) where
  encodeM (A.MetadataTuple ops) = scopeAnyCont $ do
    Context c <- gets encodeStateContext
    ops <- encodeM ops
    liftIO $ FFI.createMDNodeInContext c ops
  encodeM (A.MetadataNodeReference n) = referMDNode n

instance EncodeM EncodeAST A.MDNode (Ptr FFI.MDNode) where

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

instance DecodeM DecodeAST A.MetadataNode (Ptr FFI.MDNode) where
  decodeM p = scopeAnyCont $ do
    sId <- liftIO $ FFI.getMetadataClassId p
    case sId of
      [mdSubclassIdP|MDTuple|] -> return A.MetadataNodeReference `ap` getMetadataNodeID p
      otherwise -> do
        return A.MetadataNode `ap` decodeM p
--     -- fl <- decodeM =<< liftIO (FFI.mdNodeIsFunctionLocal p)
--     -- if fl
--     --  then
--     --    return A.MetadataNode `ap` decodeM p
--     --  else
       -- return A.MetadataNodeReference `ap` getMetadataNodeID p

getMetadataDefinitions :: DecodeAST [A.Definition]
getMetadataDefinitions = fix $ \continue -> do
  mdntd <- takeMetadataNodeToDefine
  flip (maybe (return [])) mdntd $ \(mid, p) -> do
    return (:)
      `ap` (return A.MetadataNodeDefinition `ap` return mid `ap` decodeM p)
      `ap` continue

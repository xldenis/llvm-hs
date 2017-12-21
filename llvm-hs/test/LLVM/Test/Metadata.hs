{-# LANGUAGE OverloadedStrings #-}
module LLVM.Test.Metadata where

import Test.Tasty
import Test.Tasty.HUnit

import LLVM.Test.Support

import LLVM.AST as A
import LLVM.AST.Type as A.T
import LLVM.AST.AddrSpace as A
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Visibility as V
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Constant as C
import LLVM.AST.Global as G

import LLVM.Context
import LLVM.Module

import Data.ByteString as B (readFile)

tests = testGroup "Metadata"
  [ globalMetadata
  , namedMetadata
  , nullMetadata
  , cyclicMetadata
  , diNode
  ]


diNode = testCase "dinodes" $ do
  fStr <- B.readFile "test/module.ll"
  withContext $ \context -> do
    a <- withModuleFromLLVMAssembly' context fStr moduleAST
    putStrLn (show a)
    pure ()

globalMetadata = testCase "global" $ do
    let ast = Module "<string>" "<string>" Nothing Nothing [
          GlobalDefinition $ functionDefaults {
            G.returnType = i32,
            G.name = Name "foo",
            G.basicBlocks = [
              BasicBlock (UnName 0) [
              ] (
                Do $ Ret (Just (ConstantOperand (C.Int 32 0))) [
                  ("my-metadatum", MetadataNodeReference (MetadataNodeID 0))
                ]
              )
             ]
            },
          MetadataNodeDefinition (MetadataNodeID 0) [ Just $ MDValue $ ConstantOperand (C.Int 32 1) ]
         ]
    let s = "; ModuleID = '<string>'\n\
            \source_filename = \"<string>\"\n\
            \\n\
            \define i32 @foo() {\n\
            \  ret i32 0, !my-metadatum !0\n\
            \}\n\
            \\n\
            \!0 = !{i32 1}\n"
    strCheck ast s

namedMetadata = testCase "named" $ do
    let ast = Module "<string>" "<string>" Nothing Nothing [
          NamedMetadataDefinition "my-module-metadata" [ MetadataNodeID 0 ],
          MetadataNodeDefinition (MetadataNodeID 0) [ Just $ MDValue $ ConstantOperand (C.Int 32 1) ]
         ]
    let s = "; ModuleID = '<string>'\n\
            \source_filename = \"<string>\"\n\
            \\n\
            \!my-module-metadata = !{!0}\n\
            \\n\
            \!0 = !{i32 1}\n"
    strCheck ast s

nullMetadata = testCase "null" $ do
    let ast = Module "<string>" "<string>" Nothing Nothing [
          NamedMetadataDefinition "my-module-metadata" [ MetadataNodeID 0 ],
          MetadataNodeDefinition (MetadataNodeID 0) [ Nothing ]
         ]
    let s = "; ModuleID = '<string>'\n\
            \source_filename = \"<string>\"\n\
            \\n\
            \!my-module-metadata = !{!0}\n\
            \\n\
            \!0 = !{null}\n"
    strCheck ast s

cyclicMetadata = testGroup "cyclic" [
    testCase "metadata-only" $ do
      let ast = Module "<string>" "<string>" Nothing Nothing [
            NamedMetadataDefinition "my-module-metadata" [MetadataNodeID 0],
            MetadataNodeDefinition (MetadataNodeID 0) [
              Just $ MDNode (MetadataNodeReference (MetadataNodeID 1))
             ],
            MetadataNodeDefinition (MetadataNodeID 1) [
              Just $ MDNode (MetadataNodeReference (MetadataNodeID 0))
             ]
           ]
      let s = "; ModuleID = '<string>'\n\
              \source_filename = \"<string>\"\n\
              \\n\
              \!my-module-metadata = !{!0}\n\
              \\n\
              \!0 = !{!1}\n\
              \!1 = !{!0}\n"
      strCheck ast s,

    testCase "metadata-global" $ do
      let ast = Module "<string>" "<string>" Nothing Nothing [
            GlobalDefinition $ functionDefaults {
              G.returnType = A.T.void,
              G.name = Name "foo",
              G.basicBlocks = [
                BasicBlock (UnName 0) [
                 ] (
                   Do $ Ret Nothing [ ("my-metadatum", MetadataNodeReference (MetadataNodeID 0)) ]
                 )
               ]
             },
            MetadataNodeDefinition (MetadataNodeID 0) [
              Just $ MDValue $ ConstantOperand (C.GlobalReference (ptr (FunctionType A.T.void [] False)) (Name "foo"))
             ]
           ]
      let s = "; ModuleID = '<string>'\n\
              \source_filename = \"<string>\"\n\
              \\n\
              \define void @foo() {\n\
              \  ret void, !my-metadatum !0\n\
              \}\n\
              \\n\
              \!0 = !{void ()* @foo}\n"
      strCheck ast s
   ]



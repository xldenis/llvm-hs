-- | A type to represent operands to LLVM 'LLVM.AST.Instruction.Instruction's
module LLVM.AST.Operand
( module LLVM.AST.Operand
, MetadataNodeID(..)
)
where

import LLVM.Prelude

import LLVM.AST.Name
import LLVM.AST.Constant
import LLVM.AST.InlineAssembly
import LLVM.AST.Type


import LLVM.AST.Metadata

type Metadata = Metadata' Operand
type MetadataNode = MetadataNode' Operand

-- | An 'Operand' is roughly that which is an argument to an 'LLVM.AST.Instruction.Instruction'
data Operand
  -- | %foo
  = LocalReference Type Name
  -- | 'Constant's include 'LLVM.AST.Constant.GlobalReference', for \@foo
  | ConstantOperand Constant
  | MetadataOperand Metadata
  deriving (Eq, Ord, Read, Show, Typeable, Data, Generic)

-- | The 'LLVM.AST.Instruction.Call' instruction is special: the callee can be inline assembly
type CallableOperand  = Either InlineAssembly Operand

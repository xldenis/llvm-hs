#ifndef __LLVM_INTERNAL_FFI__METADATA__H__
#define __LLVM_INTERNAL_FFI__METADATA__H__

enum {
#define HANDLE_METADATA_LEAF(CLASS) CLASS##Kind,
#include "llvm/IR/Metadata.def"
#undef HANDLE_METADATA_LEAF
} MetadataSubclassId;


  // macro(ValueAsMetadata) \
  // macro(DILexicalBlockBase) \
  // macro(DITemplateParameter) \
  // macro(DIMacroNode) \
  // macro(DIVariable) \
  // macro(DIType) \
  // macro(DILocalScope) \
  // macro(MDNode) \
  // macro(DIScope) \
  // macro(DINode) \

#define LLVM_HS_FOR_EACH_MDNODE_SUBCLASS(macro) \
  macro(MDString) \
  macro(ConstantAsMetadata) \
  macro(LocalAsMetadata) \
  macro(DistinctMDOperandPlaceholder) \
  macro(MDTuple) \
  macro(DILocation) \
  macro(DIExpression) \
  macro(DIGlobalVariableExpression) \
  macro(GenericDINode) \
  macro(DISubrange) \
  macro(DIEnumerator) \
  macro(DIBasicType) \
  macro(DIDerivedType) \
  macro(DICompositeType) \
  macro(DISubroutineType) \
  macro(DIFile) \
  macro(DICompileUnit) \
  macro(DISubprogram) \
  macro(DILexicalBlock) \
  macro(DILexicalBlockFile) \
  macro(DINamespace) \
  macro(DIModule) \
  macro(DITemplateTypeParameter) \
  macro(DITemplateValueParameter) \
  macro(DIGlobalVariable) \
  macro(DILocalVariable) \
  macro(DIObjCProperty) \
  macro(DIImportedEntity) \
  macro(DIMacro) \
  macro(DIMacroFile)

#endif

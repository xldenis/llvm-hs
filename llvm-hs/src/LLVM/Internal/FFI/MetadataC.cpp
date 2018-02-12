#define __STDC_LIMIT_MACROS

#include <iostream>
#include "llvm/Support/FormattedStream.h"

#include "llvm/Config/llvm-config.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm-c/Core.h"

using namespace llvm;

extern "C" {

LLVMMetadataRef LLVM_Hs_IsAMDString(LLVMMetadataRef md) {
    if (isa<MDString>(unwrap(md))) {
        return md;
    }
    return nullptr;
}

LLVMMetadataRef LLVM_Hs_MDStringInContext(LLVMContextRef c,
                                               const char *str, unsigned slen) {
    return wrap(MDString::get(*unwrap(c), StringRef(str, slen)));
}

const char *LLVM_Hs_GetMDString(LLVMMetadataRef md, unsigned* len) {
    if (const MDString *S = dyn_cast<MDString>(unwrap(md))) {
      *len = S->getString().size();
      return S->getString().data();
    }
    *len = 0;
    return nullptr;
}

LLVMMetadataRef LLVM_Hs_MDValue(LLVMValueRef v) {
    return wrap(ValueAsMetadata::get(unwrap(v)));
}

LLVMValueRef LLVM_Hs_MetadataOperand(LLVMContextRef c, LLVMMetadataRef md) {
    return wrap(MetadataAsValue::get(*unwrap(c), unwrap(md)));
}

LLVMMetadataRef LLVM_Hs_IsAMDNode(LLVMMetadataRef md) {
    if (isa<MDNode>(unwrap(md))) {
        return md;
    }
    return nullptr;
}

LLVMValueRef LLVM_Hs_GetMDValue(LLVMMetadataRef md) {
    return wrap(unwrap<ValueAsMetadata>(md)->getValue());
}

LLVMMetadataRef LLVM_Hs_GetMetadataOperand(LLVMValueRef val) {
    return wrap(unwrap<MetadataAsValue>(val)->getMetadata());
}

LLVMMetadataRef LLVM_Hs_MDNodeInContext(LLVMContextRef c,
                                             LLVMMetadataRef *mds,
                                             unsigned count) {
    return wrap(MDNode::get(*unwrap(c), ArrayRef<Metadata *>(unwrap(mds), count)));
}

LLVMMetadataRef LLVM_Hs_IsAMDValue(LLVMMetadataRef md) {
    if (isa<ValueAsMetadata>(unwrap(md))) {
        return md;
    }
    return nullptr;
}


LLVMValueRef LLVM_Hs_IsAMetadataOperand(LLVMValueRef val) {
    if (isa<MetadataAsValue>(unwrap(val))) {
        return val;
    }
    return nullptr;
}


unsigned LLVM_Hs_GetMDKindNames(
	LLVMContextRef c,
	const char **s,
	unsigned *l,
	unsigned n
) {
	SmallVector<StringRef, 8> ns;
	unwrap(c)->getMDKindNames(ns);
	if (ns.size() <= n) {
		for(unsigned i=0; i < ns.size(); ++i) {
			s[i] = ns[i].data();
			l[i] = ns[i].size();
		}
	}
	return ns.size();
}

unsigned LLVM_Hs_GetMDNodeNumOperands(LLVMMetadataRef v) {
	return unwrap<MDNode>(v)->getNumOperands();
}

void LLVM_Hs_NamedMetadataAddOperands(
	NamedMDNode *n,
	LLVMMetadataRef *ops,
	unsigned nOps
) {
	for(unsigned i = 0; i != nOps; ++i) n->addOperand(unwrap<MDNode>(ops[i]));
}

const char *LLVM_Hs_GetNamedMetadataName(
	NamedMDNode *n,
	unsigned *len
) {
	StringRef s = n->getName();
	*len = s.size();
	return s.data();
}

const char *LLVM_Hs_GetStringRef(
    StringRef* s,
    unsigned *len
) {
    *len = s->size();
    return s->data();
}

unsigned LLVM_Hs_GetNamedMetadataNumOperands(NamedMDNode *n) {
	return n->getNumOperands();
}

void LLVM_Hs_GetNamedMetadataOperands(NamedMDNode *n, LLVMMetadataRef *dest) {
	for(unsigned i = 0; i != n->getNumOperands(); ++i)
		dest[i] = wrap(n->getOperand(i));
}

LLVMMetadataRef LLVM_Hs_CreateTemporaryMDNodeInContext(LLVMContextRef c) {
	return wrap(MDNode::getTemporary(*unwrap(c), ArrayRef<Metadata *>()).release());
}

void LLVM_Hs_DestroyTemporaryMDNode(LLVMMetadataRef v) {
    MDNode::deleteTemporary(unwrap<MDNode>(v));
}

void LLVM_Hs_GetMDNodeOperands(LLVMMetadataRef md, LLVMMetadataRef *dest) {
    const auto *N = cast<MDNode>(unwrap(md));
    const unsigned numOperands = N->getNumOperands();
    for (unsigned i = 0; i < numOperands; i++)
        dest[i] = wrap(N->getOperand(i));
}

void LLVM_Hs_MetadataReplaceAllUsesWith(LLVMMetadataRef md, LLVMMetadataRef replacement) {
    auto *Node = unwrap<MDNode>(md);
    Node->replaceAllUsesWith(unwrap<Metadata>(replacement));
    MDNode::deleteTemporary(Node);
}

LLVMMetadataRef LLVM_Hs_IsADILocation(LLVMMetadataRef md) {
    if (isa<DILocation>(unwrap(md))) {
        return md;
    }
    return nullptr;
}

unsigned LLVM_Hs_GetMetadataClassId(LLVMMetadataRef md) {
    return (unwrap(md))->getMetadataID();
}

unsigned LLVM_Hs_DINodeGetTag(DINode *md) {
    return md->getTag();
}

unsigned LLVM_Hs_DITypeGetFlags(DIType *md) {
    return md->getFlags();
}

unsigned LLVM_Hs_DILocationGetLine(DILocation *md) {
    return md->getLine();
}

unsigned LLVM_Hs_DILocationGetColumn(DILocation *md) {
    return md->getColumn();
}

DILocalScope* LLVM_Hs_DILocationGetScope(DILocation *md) {
    return md->getScope();
}

DINode* LLVM_Hs_Get_DIEnumerator(LLVMContextRef cxt, int64_t value, const char* name) {
    LLVMContext& c = *unwrap(cxt);
    return DIEnumerator::get(c, value, MDString::get(c, name));
}

int64_t LLVM_Hs_DIEnumerator_GetValue(LLVMMetadataRef md) {
    return unwrap<DIEnumerator>(md)->getValue();
}

const char* LLVM_Hs_DIEnumerator_GetName(LLVMMetadataRef md) {
    return unwrap<DIEnumerator>(md)->getName().data();
}

MDString* LLVM_Hs_DIFileGetFilename(DIFile *di) {
    return di->getRawFilename();
}

MDString* LLVM_Hs_DIFileGetDirectory(DIFile *di) {
    return di->getRawDirectory();
}

MDString* LLVM_Hs_DIFileGetChecksum(DIFile *di) {
    return di->getRawChecksum();
}

llvm::DIFile::ChecksumKind LLVM_Hs_DIFileGetChecksumKind(DIFile *di) {
    return di->getChecksumKind();
}

// DIScope

DIScope* LLVM_Hs_DIScope_GetScope(DIScope *ds) {
    return cast_or_null<DIScope>(ds->getScope());
}

DIFile* LLVM_Hs_DIScope_GetFile(DIScope *ds) {
    return ds->getFile();
}

bool LLVM_Hs_DINamespaceGetExportSymbols(DINamespace *ds) {
    return ds->getExportSymbols();
}

DIFile* LLVM_Hs_DINamespaceGetFile(DINamespace *ds) {
    return ds->getFile();
}

const char* LLVM_Hs_DIScopeGetName(DIScope *ds, unsigned *len) {
    StringRef s = ds->getName();
    *len = s.size();
    return s.data();
}

const char* LLVM_Hs_DITypeGetName(DIType *ds, unsigned *len) {
    StringRef s = ds->getName();
    *len = s.size();
    return s.data();
}

uint64_t LLVM_Hs_DITypeGetSizeInBits(DIType *ds) {
    return ds->getSizeInBits();
}

uint64_t LLVM_Hs_DITypeGetOffsetInBits(DIType *ds) {
    return ds->getOffsetInBits();
}

uint32_t LLVM_Hs_DITypeGetAlignInBits(DIType *ds) {
    return ds->getAlignInBits();
}

unsigned LLVM_Hs_DITypeGetLine(DIType *ds) {
    return ds->getAlignInBits();
}

DIType* LLVM_Hs_Get_DIBasicType(LLVMContextRef ctx, unsigned tag, const char *name, uint64_t sizeInBits, uint32_t alignInBits, unsigned encoding) {
    LLVMContext& c = *unwrap(ctx);
    return DIBasicType::get(c, tag, MDString::get(c, name), sizeInBits, alignInBits, encoding);
}

DIFile* LLVM_Hs_Get_DIFile(LLVMContextRef ctx, const char* filename, const char* directory, unsigned checksumKind, const char* checksum) {
    LLVMContext& c = *unwrap(ctx);
    return DIFile::get(c, MDString::get(c, filename), MDString::get(c, directory), static_cast<DIFile::ChecksumKind>(checksumKind), MDString::get(c, checksum));
}

DINode* LLVM_Hs_Get_DISubrange(LLVMContextRef ctx, int64_t count, int64_t lowerBound) {
    return DISubrange::get(*unwrap(ctx), count, lowerBound);
}

int64_t LLVM_Hs_DISubrange_GetCount(DISubrange* range) {
    return range->getCount();
}

int64_t LLVM_Hs_DISubrange_GetLowerBound(DISubrange* range) {
    return range->getLowerBound();
}

unsigned LLVM_Hs_DIBasicTypeGetEncoding(DIBasicType *ds) {
    return ds->getEncoding();
}

DINodeArray LLVM_Hs_DICompositeTypeGetElements(DICompositeType *dt) {
    return dt->getElements();
}

DITypeRef LLVM_Hs_DICompositeTypeGetVTableHolder(DICompositeType *dt) {
    return dt->getVTableHolder();
}

DITypeRef LLVM_Hs_DICompositeTypeGetBaseType(DICompositeType *dt) {
    return dt->getBaseType();
}

DITypeRef LLVM_Hs_DIDerivedTypeGetBaseType(DIDerivedType *dt) {
    return dt->getBaseType();
}

unsigned LLVM_Hs_DICompositeTypeGetRuntimeLang(DICompositeType *dt) {
    return dt->getRuntimeLang();
}

DITemplateParameterArray LLVM_Hs_DICompositeTypeGetTemplateParameters(DICompositeType *dt) {
    return dt->getTemplateParams();
}

const char* LLVM_Hs_DICompositeTypeGetIdentifier(DICompositeType *dt, unsigned *len) {
    StringRef s = dt->getIdentifier();
    *len = s.size();
    return s.data();
}

DILocalScope* LLVM_Hs_DILexicalBlockBaseGetScope(DILexicalBlockBase* bb) {
    return bb->getScope();
}

unsigned LLVM_Hs_DILexicalBlockFileGetDiscriminator(DILexicalBlockFile* bf) {
    return bf->getDiscriminator();
}

unsigned LLVM_Hs_DILexicalBlockGetLine(DILexicalBlock* lb) {
    return lb->getLine();
}

unsigned LLVM_Hs_DILexicalBlockGetColumn(DILexicalBlock* lb) {
    return lb->getColumn();
}

LLVMBool LLVM_Hs_DIDerivedTypeGetAddressSpace(DIDerivedType *a, unsigned *x) {
    auto addressSpace = a->getDWARFAddressSpace();
    if (addressSpace.hasValue()) {
        *x = addressSpace.getValue();
        return 1;
    } else {
        return 0;
    }
}

uint8_t LLVM_Hs_DISubroutineTypeGetCC(DISubroutineType *a) {
    return a->getCC();
}

void LLVM_Hs_GetDISubroutineTypeArray(DISubroutineType *md, DITypeRef *dest) {
    auto arr = md->getTypeArray();
    const unsigned numOperands = arr.size();
    for (unsigned i = 0; i < numOperands; i++)
        dest[i] = arr[i];
}

unsigned LLVM_Hs_DISubroutineTypeArrayLength(DISubroutineType *a) {
    return a->getTypeArray().size();
}

// DISubprogram

unsigned LLVM_Hs_DISubprogram_GetLine(DISubprogram* p) {
    return p->getLine();
}

unsigned LLVM_Hs_DISubprogram_GetVirtuality(DISubprogram* p) {
    return p->getVirtuality();
}

unsigned LLVM_Hs_DISubprogram_GetVirtualIndex(DISubprogram* p) {
    return p->getVirtualIndex();
}

unsigned LLVM_Hs_DISubprogram_GetScopeLine(DISubprogram* p) {
    return p->getScopeLine();
}

LLVMBool LLVM_Hs_DISubprogram_IsOptimized(DISubprogram* p) {
    return p->isOptimized();
}

LLVMBool LLVM_Hs_DISubprogram_IsDefinition(DISubprogram* p) {
    return p->isDefinition();
}

// DIExpression

unsigned LLVM_Hs_DIExpression_GetNumElements(DIExpression* e) {
    return e->getNumElements();
}

unsigned LLVM_Hs_DIExpression_GetElement(DIExpression* e, unsigned i) {
    fprintf(stderr, "%p, %d, %d\n", (void*)e, i, e->getNumElements());
    return e->getElement(i);
}

// DIVariable

DIScope* LLVM_Hs_DIVariable_GetScope(DIVariable* v) {
    return v->getScope();
}

DIFile* LLVM_Hs_DIVariable_GetFile(DIVariable* v) {
    return v->getFile();
}

const char* LLVM_Hs_DIVariable_GetName(DIVariable* v) {
    return v->getName().data();
}

unsigned LLVM_Hs_DIVariable_GetLine(DIVariable* v) {
    return v->getLine();
}

DIType* LLVM_Hs_DIVariable_GetType(DIVariable* v) {
    return v->getType().resolve();
}

// DICompileUnit
DICompileUnit* LLVM_Hs_Get_DICompileUnit
  (LLVMContextRef ctx,
   unsigned sourceLanguage, DIFile* file, const char* producer, LLVMBool isOptimized, const char* flags,
   unsigned runtimeVersion, const char* splitDebugFilename, unsigned emissionKind, Metadata* enumTypes, Metadata* retainedTypes,
   Metadata* globalVariables, Metadata* importedEntities, Metadata* macros, uint64_t dwoid, LLVMBool splitDebugInlining,
   LLVMBool debugInfoForProfiling) {
    LLVMContext &c = *unwrap(ctx);
    return DICompileUnit::getDistinct
        (c,
         sourceLanguage, file, MDString::get(c, producer), isOptimized, MDString::get(c, flags),
         runtimeVersion, MDString::get(c, splitDebugFilename), emissionKind, enumTypes, retainedTypes,
         globalVariables, importedEntities, macros, dwoid, splitDebugInlining,
         debugInfoForProfiling);
}

unsigned LLVM_Hs_DICompileUnit_GetLanguage(DICompileUnit* cu) {
    return cu->getSourceLanguage();
}

LLVMBool LLVM_Hs_DICompileUnit_GetSplitDebugInlining(DICompileUnit* cu) {
    return cu->getSplitDebugInlining();
}

LLVMBool LLVM_Hs_DICompileUnit_GetDebugInfoForProfiling(DICompileUnit* cu) {
    return cu->getDebugInfoForProfiling();
}

LLVMBool LLVM_Hs_DICompileUnit_GetOptimized(DICompileUnit* cu) {
    return cu->isOptimized();
}

unsigned LLVM_Hs_DICompileUnit_GetRuntimeVersion(DICompileUnit* cu) {
    return cu->getRuntimeVersion();
}

const char* LLVM_Hs_DICompileUnit_GetProducer(DICompileUnit* cu) {
    const char* producer = cu->getProducer().data();
    return producer ? producer : "";
}

const char* LLVM_Hs_DICompileUnit_GetFlags(DICompileUnit* cu) {
    const char* flags = cu->getFlags().data();
    return flags ? flags : "";
}

const char* LLVM_Hs_DICompileUnit_GetSplitDebugFilename(DICompileUnit* cu) {
    const char* filename = cu->getSplitDebugFilename().data();
    return filename ? filename : "";
}

unsigned LLVM_Hs_DICompileUnit_GetEmissionKind(DICompileUnit* cu) {
    return cu->getEmissionKind();
}

uint64_t LLVM_Hs_DICompileUnit_GetDWOId(DICompileUnit* cu) {
    return cu->getDWOId();
}

}


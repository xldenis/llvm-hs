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

int64_t LLVM_Hs_DIEnumeratorGetValue(LLVMMetadataRef md) {
    return unwrap<DIEnumerator>(md)->getValue();
}

MDString* LLVM_Hs_DIEnumeratorGetName(LLVMMetadataRef md, unsigned *len) {
    return unwrap<DIEnumerator>(md)->getRawName();
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

DIScope* LLVM_Hs_DIScopeGetScope(DIScope *ds) {
    return cast_or_null<DIScope>(ds->getScope());
}

DIFile* LLVM_Hs_DIScopeGetFile(DIScope *ds) {
    return cast_or_null<DIFile>(ds->getFile());
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

}


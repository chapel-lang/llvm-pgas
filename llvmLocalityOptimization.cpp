/*
 * Copyright 2004-2015 Cray Inc.
 * Other additional copyright holders may be indicated within.
 * 
 * The entirety of this work is licensed under the Apache License,
 * Version 2.0 (the "License"); you may not use this file except
 * in compliance with the License.
 * 
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//===----------------------------------------------------------------------===//
// Chapel LLVM Locality Optimization by Akihiro Hayashi (ahayashi@rice.edu)
//===----------------------------------------------------------------------===//
// LLVM-based Locality Inference Pass (Locality Optimization Pass)
// This pass tries to convert possibly-remote access (addrspace(100)* access)
// to definitely-local to avoid runtime affinity checking overheads.
//
// To infer the locality, the locality optimization pass tries to utilize
// following information :
// - Case 1. Scalar access enclosed by Chapel's LOCAL statement.
//   proc localizeByLocalStmt(ref x) : int {
//     var p: int = 1;
//     local { p = x; }
//     return p + x; // x is definitely local
//   }
//   The locality level of x is inferred by searching SSA value graph,
//   which is implemented in IGraph.[h|cpp].
//   When you specify debugThisFn, the pass generates .dot file
//   that can be visualized by the graphviz tool. (http://www.graphviz.org/)
//
// - Case 2. Array access enclosed by Chapel's LOCAL statement.
//   proc habanero(A) : int {
//     A(1) = 1; // A(1) is definitely local
//     local { A(1) = 2; }
//     A(2) = 3; // A(2) is possibly remote
//   }
//   This pass is element-sensitive. For example,
//   the locality of A(1) is "definitely-local",
//   but the pass leave A(2) "possibly-remote" since there is no enough
//   information about the locality of A(2). 
//   This is done by using a reduced version of the LLVM's global value numbering
//   pass (in ValueTable.[h|cpp]) and a array offset analysis.
//
// - Case 3. locale locale array declaration
//   proc localizeByArrayDecl () {
//     var A: [1..10] int;
//     return A(5);
//    }
//   The locality of A(5) is "definitely-local" since an array A is declared in this scope.
//   Note that this pass is not element-sensitve so far. 
//
// Limitation, TODOs and future work:
//   (Limitation) Locality Inference using SSA Value Graph with if statements:
//     The current implementation does not propagate a condition even if a local statement is enclosed by if statement.
//     Hence, we may fail to infer the locality in some cases.
//     (e.g. if (condition) { local{ p = x } })
//     
//   (Limitation) Chapel's local statement detection:
//     Currently, we are assuming that gf.addr function calls correspond to Chapel's local statements,
//     but this is not always true because gf.addr is also used to extract a local pointer from a wide pointer.
//     To avoid this problem, we have an std::vector named "NonLocals" to record a retun value of gf.addr
//     which is also an argument of gf.make and the NonLocals are referred when doing "exemptionTest".
//     This may not be always true. Ideally, a PGAS-LLVM frontend should tell the locality optimization pass
//     which gf.addr call is a local statement.
//
//      Example :
//        1. call i64* @.gf.addr.1(i64 addrspace(100)* %x)       // %x is definitely local
//        2. %y = call i64* @.gf.addr.1(i64 addrspace(100)* %x)  // might not be definitely local
//	     call i64 addrspace(100)* @.gf.make.1(..., %y) 
//     
//   (Limitation) Chapel's Array Declaration detection:
//     We basically look for chpl__convertRuntimeTypeToValue to detect Chapel's array declaration.
//     This pattern matching completely depends on how PGAS-LLVM frontend emits LLVM IR.
//     Please see analyzeCallInsn for more details.
//
//   (Limitation) Intra-procedural pass:
//     Unfortunately, the current implementation is not inter-procedural. 
//
//   (Future Work) The utilization of high-level information:
//     The locality optimization pass has to recover high-level information such as 
//     array accesses and local statements from low-level LLVM IR, but ideally,
//     PGAS-LLVM frontend are supposed to add annotations to keep these information
//     so the locality optimization can perform language-agnostic PGAS optimization.
//
//===----------------------------------------------------------------------===//

#include "llvmLocalityOptimization.h"

#ifdef HAVE_LLVM

#define DEBUG_TYPE "locality-opt"

#include "llvmUtil.h"
#include <cstdio>
#include <iostream>

#if HAVE_LLVM_VER >= 35
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Verifier.h"
#else
#include "llvm/Support/InstIterator.h"
#include "llvm/Support/CallSite.h"
#include "llvm/Analysis/Verifier.h"
#endif

// For Debugging 
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"

#include "llvm/ADT/GraphTraits.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/ValueMapper.h"
#include "llvm/LinkAllPasses.h"

#include "llvm/Transforms/Utils/Cloning.h"

#include "llvmGlobalToWide.h"
#include "IGraph.h"
#include "ValueTable.h"

using namespace llvm;

namespace {

    // For Debug
    static bool debugPassInsn = true;
    static const bool extraChecks = false;
    static const char* debugThisFn = "habanero";

    // For Chapel Compiler & Breakdown
    static const bool fLLVMDisableIG = false;
    static const bool fLLVMDisableDecl = false;
    static const bool fLLVMDisableGVN = false;
    static const bool fLLVMLocalityOpt = true;

    // Statistics
    STATISTIC(NumLocalizedByIG, "Number of localized operations by IG");
    STATISTIC(NumLocalizedByGVN, "Number of localized operations by GVN");
    STATISTIC(NumLocalizedByArrayDecl, "Number of localized operations by Locale-local Array");
        
    struct LocalityOpt : public ModulePass {

	static char ID;
	
	GlobalToWideInfo *info;
	std::string layoutAfterwards;
	
	LocalityOpt(GlobalToWideInfo* _info, std::string layout) 
	    : ModulePass(ID), info(_info), layoutAfterwards(layout) {	    
	}
	
	// Constructor for running within opt, for testing and
	// bugpoint.
	LocalityOpt()
	    : ModulePass(ID), info(NULL), layoutAfterwards("") {
	}
	
	class LocalArrayEntry {
	private:
	    Value *op;
	    bool whole;
	    vector<unsigned int> localOffsets;
	public:
	    LocalArrayEntry(Value* _op, bool _whole) : op(_op), whole(_whole) {}
	    void addLocalOffset(unsigned int offs) {
		for (vector<unsigned int>::iterator I = localOffsets.begin(), E = localOffsets.end(); I != E; I++) {
		    if (offs == *I) {
			return;
		    }
		}
		localOffsets.push_back(offs);
	    }
	    Value* getOp() { return op; }
	    void dumpLocalOffsets() {
		for (vector<unsigned int>::iterator I = localOffsets.begin(), E = localOffsets.end(); I != E; I++) {
		    errs () << *I << ", ";
		}
		errs () << "\n";
	    }
	    bool isLocalOffset(int offset) {
		if (std::find(localOffsets.begin(), localOffsets.end(), offset) != localOffsets.end()) {
		    return true;
		}
		return false;
	    }      	    
	    bool isWholeLocal() { return whole; }
	};

	class LocalArrayInfo {
	private:
	    vector<LocalArrayEntry*> list;
	public:
	    LocalArrayInfo() {}
	    void add(LocalArrayEntry *li) { list.push_back(li); }
	    LocalArrayEntry* getEntryByValue(const Value *op) {
		for (vector<LocalArrayEntry*>::iterator I = list.begin(), E = list.end(); I != E; I++) {
		    LocalArrayEntry *li = *I;
		    if (li->getOp() == op) {
			return li;
		    }
		}
		return NULL;
	    }
	    void dump() {
		errs () << "[Local Array Info Start]\n";
		for (vector<LocalArrayEntry*>::iterator I = list.begin(), E = list.end(); I != E; I++) {
		    LocalArrayEntry *li = *I;
		    errs () << *(li->getOp()) << "\n";
		    errs () << "Definitely Local Offset : ";
		    if (li->isWholeLocal()) {
			errs () << "WHOLE\n";
		    } else {
			li->dumpLocalOffsets();
		    }
		}
		errs () << "[Local Array Info End]\n";
	    }
	};

#if HAVE_LLVM_VER >= 35
	void dumpFunction(Function *F, std::string mid) {
	    std::string Filename = F->getName().str() + "." + mid + ".ll";
	    std::error_code EC;
	    raw_fd_ostream File(Filename.c_str(), EC, sys::fs::F_Text);
	    
	    if (EC) {
		errs() << "Dump Function : error: "<< EC.message() << "\n";		
	    } else {
		File << *F;
	    }
	}
#else
        void dumpFunction(Function *F, std::string mid) {
	    std::string Filename = F->getName().str() + "." + mid + ".ll";
	    std::string ErrorInfo;
	    raw_fd_ostream File(Filename.c_str(), ErrorInfo);

	    if (ErrorInfo.empty())
		File << *F;
	    else
		errs() << "  error opening file for writing!";
	    errs() << "\n";
	}
#endif
	// For Debugging purpose
	void insertPrintf(Module &M, Instruction *insertBefore, StringRef Str) {
	    // Global Value
	    Constant *StrConstant = ConstantDataArray::getString(M.getContext(), Str);
	    GlobalVariable *GV = new GlobalVariable(M, StrConstant->getType(), true, GlobalValue::PrivateLinkage, StrConstant);
	    GV->setUnnamedAddr(true);
	    // GEP
	    Value *zero = ConstantInt::get(Type::getInt32Ty(M.getContext()), 0);
	    Value *gepArgs[] = { zero, zero };
	    Instruction *gepInst = GetElementPtrInst::CreateInBounds(GV, gepArgs, "", insertBefore);
	    // Printf
	    Constant *putsFunc = M.getOrInsertFunction("puts", Type::getInt32Ty(M.getContext()), Type::getInt8PtrTy(M.getContext()), NULL);
	    Value* printfArgs[1];
	    printfArgs[0] = gepInst;
	    CallInst::Create(putsFunc, printfArgs, "", insertBefore);
	}

	bool isaGlobalPointer(GlobalToWideInfo* info, Type* type) {
	    PointerType* pt = dyn_cast<PointerType>(type);
	    if( pt && pt->getAddressSpace() == info->globalSpace ) return true;
	    return false;
	}

	void createValueTableInsn(ValueTable *vn, Instruction *insn) {
	    if (insn->getType()->isVoidTy()) return;
	    vn->lookup_or_add(insn);
	}
	
	ValueTable* createValueTable(Function *F) {
	    ValueTable *vn = new ValueTable();
	    for (inst_iterator II = inst_begin(F), IE = inst_end(F); II != IE; ++II) {
		Instruction *insn = &*II;
		createValueTableInsn(vn, insn);
	    }
	    return vn;
	}

	bool isDefinitelyLocalAccordingToIG(Value* op, Instruction *insn, IGraph *G) {
	    if (fLLVMDisableIG) {
		return false;
	    }
	    IGraph::Answer answer = G->prove(op, insn, 0);
	    if (answer == IGraph::TRUE) {
		NumLocalizedByIG++;
		return true;
	    } else {
		return false;
	    }
	}

	// Assuming op is operand of GEP inst (e.g. getelementptr inbounds i64, i64 addrspace(100)* op)
	// find array access and localize it if array descriptor is definitely local according to GVN info
	bool isDefinitelyLocalAccordingToList(GetElementPtrInst* oldGEP, ValueToValueMapTy &VM, IGraph *G, LocalArrayInfo *LocalArrays, bool isGVN) {

	    if (isGVN && fLLVMDisableGVN) {
		return false;
	    }
	    if (!isGVN && fLLVMDisableDecl) {
		return false;
	    }

	    Value *op = oldGEP->getPointerOperand();
	    if (!op) {
		return false;
	    }
	    LocalArrayEntry *local = LocalArrays->getEntryByValue(op);

	    // First Step : See if this GEP access is array access. If so, see if a pointer to array is in LocalArrayInfo  
	    bool possiblyLocal = false;
	    bool definitelyLocal = false;
	    int offset = -1;

	    // Case 1 : this is GEP LocalArray, 0, 8 
	    if (local) {		
		possiblyLocal = true;
	    }

	    // Case 2 : this GEP is obaining a pointer to array element. (GEP %op, %offset)
	    // Searching (GEP array, 0, 8) and see if array is in local LocalArray
	    if (isa<LoadInst>(op)) {
		LoadInst *loadInst = cast<LoadInst>(op);
		GetElementPtrInst *gepInst = dyn_cast<GetElementPtrInst>(loadInst->getPointerOperand());
		if (gepInst && gepInst->getNumIndices() == 2) {
		    Constant *op1 = dyn_cast<Constant>(gepInst->getOperand(1));
		    Constant *op2 = dyn_cast<Constant>(gepInst->getOperand(2));
		    if (op1 != NULL && op2 != NULL
			&& op1->getUniqueInteger() == 0 && op2->getUniqueInteger() == 8) {
			// $shifteddata = GEP %arraydesciptor, 0 , 8
			// original GEP is supposed to be array access (GEP %shifteddata, %offset)

			// search array descriptor 
			LocalArrayEntry *li1 = LocalArrays->getEntryByValue(gepInst->getPointerOperand());
			if (li1) {
			    possiblyLocal = true;
			    local = li1;
			}
			// search key assuming array descriptor has already been renamed.
			const GetElementPtrInst *keyGep = NULL;
			for (ValueToValueMapTy::iterator I = VM.begin(), E = VM.end(); I != E; I++) {
			    if (I->second == gepInst) {
				keyGep = cast<GetElementPtrInst>(I->first);
				break;
			    }
			}
			if (keyGep != NULL) {
			    // this GEP is definitely array offset calculation
			    const Value* v = keyGep->getPointerOperand();  
			    LocalArrayEntry *li2 = LocalArrays->getEntryByValue(v);
			    if (li2) {
				possiblyLocal = true;
				local = li2;
			    }
			}
		    }
		}	      
	    }

	    // putting it together
	    if (possiblyLocal) {
		if (!local || local->isWholeLocal()) {
		    definitelyLocal = true;
		} else {
		    // Check if this offset is local
		    offset = analyzeArrayAccessOffsets(oldGEP);
		    errs () << "Offset Fire: " << offset << "\n";
		    if (!local || local->isLocalOffset(offset)) {
			definitelyLocal = true;
		    } else {
			definitelyLocal = false;
		    }
		}
	    } else {
		definitelyLocal = false;
	    }

	    if (definitelyLocal) {
		if (isGVN) {
		    NumLocalizedByGVN++;
		} else {
		    NumLocalizedByArrayDecl++;
		}
	    }
	    return definitelyLocal;
	}
	
	Value* findNewOpOrInsertGF(Value *oldOp, ValueToValueMapTy &VM, Module &M, Instruction *insertBefore) {
	    Value *tmpOp, *newOp;
	    // check mapping
	    ValueToValueMapTy::iterator I = VM.find(oldOp);
	    if (I != VM.end() && I->second) {
		tmpOp = I->second;
	    } else {
		tmpOp = oldOp;
	    }
	    Type* t = tmpOp->getType();
	    if (t->isPointerTy() && t->getPointerAddressSpace() == info->globalSpace) {
		// create gf.addr.
		PointerType *addrType = cast<PointerType>(oldOp->getType());
		assert(addrType != NULL);
		if (addrType->getPointerElementType()->isPointerTy()) {
		    newOp = tmpOp;
		    if (debugPassInsn) {
			errs() << "GF is not inserted\n";
		    }
		} else {
		    Function* fn = getAddrFn(&M, info, addrType);
		    Value* gf_addr_args[1];
		    gf_addr_args[0] = tmpOp;
		    newOp = CallInst::Create(fn, gf_addr_args, "", insertBefore);
		    if (debugPassInsn) {
			errs() << "GF Inserted : " << *newOp << "\n";
		    }
		}
	    } else {
		newOp = tmpOp;
	    } 
	    return newOp;
	}

	Instruction* duplicateCallInst(CallInst *oldCall, Function* newF) {
	    Instruction *newCall;
	    CallSite CS(oldCall);
	    const AttributeSet &CallPAL = CS.getAttributes();
	    SmallVector<Value*, 16> args;

	    for (unsigned int i = 0; i < oldCall->getNumArgOperands(); i++) {
		Value *op = oldCall->getArgOperand(i);
		args.push_back(op);
	    }
	    
	    if (InvokeInst *II = dyn_cast<InvokeInst>(oldCall)) {
		newCall = InvokeInst::Create(newF, II->getNormalDest(), II->getUnwindDest(),
					     args, "", oldCall);
		cast<InvokeInst>(newCall)->setCallingConv(CS.getCallingConv());
		cast<InvokeInst>(newCall)->setAttributes(CallPAL);
	    } else {
		newCall = CallInst::Create(newF, args, "", oldCall);
		cast<CallInst>(newCall)->setCallingConv(CS.getCallingConv());
		cast<CallInst>(newCall)->setAttributes(CallPAL);
		if (cast<CallInst>(oldCall)->isTailCall())
		    cast<CallInst>(newCall)->setTailCall();
	    }
	    if (MDNode *tbaa = oldCall->getMetadata(LLVMContext::MD_tbaa)) {
		newCall->setMetadata(LLVMContext::MD_tbaa, tbaa);
	    }
	    if (MDNode *tbaaStruct = oldCall->getMetadata(LLVMContext::MD_tbaa_struct)) {
		newCall->setMetadata(LLVMContext::MD_tbaa_struct, tbaaStruct);
	    }
	    return newCall;
	}

	bool checkNeedToWork(Instruction *insn, ValueToValueMapTy &VM) {
	    bool needsWork = false;
	    for(unsigned int i=0; i < insn->getNumOperands(); i++) {
		Value *old = insn->getOperand(i);
		ValueToValueMapTy::iterator I = VM.find(old);
		if( I != VM.end() && I->second ) needsWork = true;
	    }
	    
	    // check global
	    if (insn->getOpcode() != Instruction::Call) {
		for(unsigned int i=0; i < insn->getNumOperands(); i++) {
		    Value *old = insn->getOperand(i);
		    if( isaGlobalPointer(info, old->getType()) ) needsWork = true;
		}
		if( isaGlobalPointer(info, insn->getType()) ) needsWork = true;
	    } else {
		CallInst *call = cast<CallInst>(insn);
		Function *F = call->getCalledFunction();
		if (!F) return false;
		if (isa<MemIntrinsic>(call) && isa<MemCpyInst>(call)) {
		    Value* gDst = call->getArgOperand(0);
		    Value* gSrc = call->getArgOperand(1);
		    if (gDst->getType()->getPointerAddressSpace() == info->globalSpace
			|| gSrc->getType()->getPointerAddressSpace() == info->globalSpace) {
			needsWork = true;
		    }			
		} else if (F->getName().startswith(".gf.addr")) { 
		    needsWork = true;
		}
	    }
	    return needsWork;
	}

	void processInstruction(Instruction* targetInsn, SmallVector<Instruction*, 16> &deletedInsn, ValueToValueMapTy &VM, ValueTable *VN, Module &M, IGraph *G, LocalArrayInfo *LocalArraysGVN, LocalArrayInfo *LocalArraysDecl) {
	    if(debugPassInsn) {
		errs() << "@" << *targetInsn << "\n";
	    }
	    
	    bool needsWork = checkNeedToWork(targetInsn, VM);
	    if (!needsWork) {
		if (debugPassInsn) { 
		    errs() << "need not to work!\n"; 
		}
		return;
	    }
	    
	    switch(targetInsn->getOpcode()) {
	    case Instruction::PHI: { /* TODO : Consider PHI Node */ break; }
	    case Instruction::BitCast: {
		CastInst *oldCast = cast<CastInst>(targetInsn);
		Value* op = oldCast->getOperand(0);
		ValueToValueMapTy::iterator I = VM.find(op);
		if (I != VM.end() && I->second) {
		    Value* newOp = I->second;
		    if (debugPassInsn) {
			errs () << "Take a look :" << *newOp->getType() << "\n";
		    }
		    Type* oldSrcTy = oldCast->getSrcTy();
		    Type* newSrcTy = newOp->getType();
		    Type* oldDstTy = oldCast->getDestTy();
		    assert(oldSrcTy->isPointerTy() && newSrcTy->isPointerTy() && oldDstTy->isPointerTy());
		    bool srcIsWide = newSrcTy->getPointerAddressSpace() == 0;
		    bool dstIsGlobal = oldDstTy->getPointerAddressSpace() == info->globalSpace;
		    if (srcIsWide && dstIsGlobal) {
			Type* newDstTy = convertTypeGlobalToWide(&M, info, oldDstTy);
			Instruction* newInst = CastInst::Create(oldCast->getOpcode(), newOp, newDstTy, "", oldCast);
			if (debugPassInsn) {
			    errs() << "Old Instruction : " << *oldCast << "\n";
			    errs() << "New Instruction : " << *newInst << "\n";
			}
			VM[oldCast] = newInst;
			deletedInsn.push_back(oldCast);
		    } else {
			RemapInstruction(targetInsn, VM, RF_IgnoreMissingEntries);
			if (debugPassInsn) {
			    errs() << "New Instruction : " << *targetInsn << "\n";
			}
		    }
		} else {
		    if (debugPassInsn) {
			errs () << "No transformation\n";
		    }
		}
		break;
	    }
	    case Instruction::GetElementPtr: {
		GetElementPtrInst *oldGEP = cast<GetElementPtrInst>(targetInsn);
		if (oldGEP->getAddressSpace() == info->globalSpace) {
		    Value *oldOp, *newOp;
		    Instruction* newInst = NULL;
		    // Old Operand addrspace(100)*
		    oldOp = oldGEP->getPointerOperand();
		    bool needToTransform = false;
		    // For array access
		    // Check if the pointer is definitely local (according to inequality graph)
		    needToTransform |= isDefinitelyLocalAccordingToIG(oldOp, targetInsn, G);
		    // Check if the pointer derives from locale-local array pointer (according to GVN)
		    needToTransform |= isDefinitelyLocalAccordingToList(oldGEP, VM, G, LocalArraysGVN, true);
		    // Check if the pointer derives from locale-local array pointer (according to locale-local array)
		    needToTransform |= isDefinitelyLocalAccordingToList(oldGEP, VM, G, LocalArraysDecl, false);
		    // 
		    ValueToValueMapTy::iterator I = VM.find(oldOp);
		    needToTransform |= (I != VM.end() && I->second);
		    if (needToTransform) {
			newOp = findNewOpOrInsertGF(oldOp, VM, M, oldGEP);
			// creating new GEP
			std::vector<Value *> args;
			for (User::op_iterator OI = oldGEP->idx_begin(), OE = oldGEP->idx_end(); OI != OE; OI++) {
			    args.push_back(*OI);
			}
			ArrayRef<Value*> argsRef(args);
			// Create new GEP
			bool inBounds = oldGEP->isInBounds();
			if (inBounds) {
			    newInst = GetElementPtrInst::CreateInBounds(newOp, argsRef, oldGEP->getName(), oldGEP);
			} else {
#if HAVE_LLVM_VER >= 35
			    newInst = GetElementPtrInst::Create(newOp->getType(), newOp, argsRef, oldGEP->getName(), oldGEP);
#else
			    newInst = GetElementPtrInst::Create(newOp, argsRef, oldGEP->getName(), oldGEP);
#endif			    
			}			
			if (debugPassInsn) {
			    errs() << "Old Instruction : " << *oldGEP << "\n";
			    errs() << "New Instruction : " << *newInst << "\n";
			}
			// TODO: reconsider return type of GEP
			VM[oldGEP] = newInst;
			deletedInsn.push_back(oldGEP);
		    }
		} else {
		    RemapInstruction(oldGEP, VM, RF_IgnoreMissingEntries);
		}
		break;
	    }
	    case Instruction::Load: {
		LoadInst *oldLoad = cast<LoadInst>(targetInsn);
		RemapInstruction(oldLoad, VM, RF_IgnoreMissingEntries);
		if(oldLoad->getPointerAddressSpace() == info->globalSpace) {
		    Value *oldOp, *newOp;
		    Instruction* newInst = NULL;
		    // Old Operand addrspace(100)*
		    oldOp = oldLoad->getPointerOperand();
		    if (isDefinitelyLocalAccordingToIG(oldOp, targetInsn, G)) {
			newOp = findNewOpOrInsertGF(oldOp, VM, M, oldLoad);
			newInst = new LoadInst(newOp, 
					       "", 
					       oldLoad->isVolatile(), 
					       oldLoad->getAlignment(),
					       oldLoad->getOrdering(), 
					       oldLoad->getSynchScope(), 
					       oldLoad);
			if (MDNode *tbaa = oldLoad->getMetadata(LLVMContext::MD_tbaa)) {
			    newInst->setMetadata(LLVMContext::MD_tbaa, tbaa);
			}
			if (debugPassInsn) {
			    errs() << "Old Instruction : " << *oldLoad << "\n";
			    errs() << "New Instruction : " << *newInst << "\n";
			}
			if (!newInst->getType()->isPointerTy()) {
			    oldLoad->replaceAllUsesWith(newInst);
			}
			VM[oldLoad] = newInst;
			deletedInsn.push_back(oldLoad);
		    }
		}
		break; 
	    }
	    case Instruction::Store: {
		StoreInst *oldStore = cast<StoreInst>(targetInsn);
		RemapInstruction(oldStore, VM, RF_IgnoreMissingEntries);
		if (oldStore->getPointerAddressSpace() == info->globalSpace) {
		    Value *oldOp, *newOp;
		    Instruction* newInst = NULL;
		    // Old Operand addrspace(100)*			
		    oldOp = oldStore->getPointerOperand();
		    if (isDefinitelyLocalAccordingToIG(oldOp, targetInsn, G)) {
			newOp = findNewOpOrInsertGF(oldOp, VM, M, oldStore);
			newInst = new StoreInst(oldStore->getValueOperand(), 
						newOp, 
						oldStore->isVolatile(), 
						oldStore->getAlignment(), 
						oldStore->getOrdering(), 
						oldStore->getSynchScope(), 
						oldStore);
			if (MDNode *tbaa = oldStore->getMetadata(LLVMContext::MD_tbaa)) {
			    newInst->setMetadata(LLVMContext::MD_tbaa, tbaa);
			}
			if (debugPassInsn) {
			    errs() << "Old Instruction : " << *oldStore << "\n";
			    errs() << "New Instruction : " << *newInst << "\n";
			}
			VM[oldStore] = newInst;
			deletedInsn.push_back(oldStore);
		    }
		} 
		break; 
	    }
	    case Instruction::Call: {
		CallInst *oldCall = cast<CallInst>(targetInsn);
		Function* oldF = oldCall->getCalledFunction(); // null if indirect
		assert(oldF != NULL);
		if (oldF->getName().startswith(".gf.addr")) {
		    Value *op = oldCall->getArgOperand(0);
		    ValueToValueMapTy::iterator I = VM.find(op);
		    if (I != VM.end() && I->second) {
			Value *addrOp = I->second;	
			// TODO check
			errs () << "Removing gf.addr : ";
			op->getType()->dump(); 
			errs () << " => ";
			addrOp->getType()->dump();
			errs () << "\n";
			VM[oldCall] = addrOp;
			deletedInsn.push_back(oldCall);
			errs () << "gf.addr removed\n";
		    }
		    break;	    
		} else if (isa<MemIntrinsic>(oldCall)) {
		    if (isa<MemSetInst>(oldCall)) {
			Value *oldDst = oldCall->getArgOperand(0);			
			ValueToValueMapTy::iterator I = VM.find(oldDst);
			if (I != VM.end() && I->second) {
			    Value* newDst = I->second;
			    CallSite CS(oldCall);
			    const AttributeSet &CallPAL = CS.getAttributes();
			    Type *types[2];
			    Value *args[5];

			    types[0] = newDst->getType();
			    types[1] = oldCall->getArgOperand(2)->getType();

			    args[0] = newDst;
			    args[1] = oldCall->getArgOperand(1);
			    args[2] = oldCall->getArgOperand(2);
			    args[3] = oldCall->getArgOperand(3);
			    args[4] = oldCall->getArgOperand(4);

			    Function* memSetF = Intrinsic::getDeclaration(&M, Intrinsic::memset, types);
			    Instruction* newCall = CallInst::Create(memSetF, args, "", oldCall);
			    cast<CallInst>(newCall)->setCallingConv(CS.getCallingConv());
			    cast<CallInst>(newCall)->setAttributes(CallPAL);
			    if (cast<CallInst>(oldCall)->isTailCall()) {
				cast<CallInst>(newCall)->setTailCall();
			    }
			    if (MDNode *tbaa = oldCall->getMetadata(LLVMContext::MD_tbaa)) {
				newCall->setMetadata(LLVMContext::MD_tbaa, tbaa);
			    }
			    if (MDNode *tbaaStruct = oldCall->getMetadata(LLVMContext::MD_tbaa_struct)) {
				newCall->setMetadata(LLVMContext::MD_tbaa_struct, tbaaStruct);
			    }
			    if (debugPassInsn) {
				errs () << "MemSet Old Instruction : " << *oldCall << "\n";
				errs () << "New Instruction : " << *newCall << "\n";
			    }
			    VM[oldCall] = newCall;
			    deletedInsn.push_back(oldCall);			
			} else {
			    /* do nothing */
			}
			break;
		    }
		    assert(isa<MemCpyInst>(oldCall) || isa <MemMoveInst>(oldCall));
		    Value *newDst, *newSrc;
		    Value* oldDst = oldCall->getArgOperand(0);
		    Value* oldSrc = oldCall->getArgOperand(1);
		    bool needToTransform = false;

		    unsigned dstSpace = oldDst->getType()->getPointerAddressSpace();
		    unsigned srcSpace = oldSrc->getType()->getPointerAddressSpace();

		    CallSite CS(oldCall);
		    const AttributeSet &CallPAL = CS.getAttributes();
		    Type *types[3];
		    Value *args[5];
		    
		    if (srcSpace == info->globalSpace) {
			ValueToValueMapTy::iterator I = VM.find(oldSrc);
			bool renamed = I != VM.end() && I->second;
			if (isDefinitelyLocalAccordingToIG(oldSrc, targetInsn, G) || renamed) {
			    newSrc = findNewOpOrInsertGF(oldSrc, VM, M, oldCall);
			    needToTransform = true;
			}
		    } else {
			newSrc = oldSrc;
		    }
		    if (dstSpace == info->globalSpace) {
			ValueToValueMapTy::iterator I = VM.find(oldDst);
			bool renamed = I != VM.end() && I->second;
			if (isDefinitelyLocalAccordingToIG(oldDst, targetInsn, G) || renamed) {
			    newDst = findNewOpOrInsertGF(oldDst, VM, M, oldCall);
			    needToTransform = true;
			}
		    } else {
			newDst = oldDst;
		    }

		    if (!needToTransform) {
			break;
		    }

		    types[0] = newDst->getType();
		    types[1] = newSrc->getType();
		    types[2] = oldCall->getArgOperand(2)->getType();

		    args[0] = newDst;
		    args[1] = newSrc;
		    args[2] = oldCall->getArgOperand(2);
		    args[3] = oldCall->getArgOperand(3);
		    args[4] = oldCall->getArgOperand(4);

		    Function* memF = NULL;
		    if (isa<MemCpyInst>(oldCall)) {
			memF = Intrinsic::getDeclaration(&M, Intrinsic::memcpy, types);
		    } else if (isa <MemMoveInst>(oldCall)) {
			memF = Intrinsic::getDeclaration(&M, Intrinsic::memmove, types);
		    }
		    Instruction* newCall = CallInst::Create(memF, args, "", oldCall);
		    cast<CallInst>(newCall)->setCallingConv(CS.getCallingConv());
		    cast<CallInst>(newCall)->setAttributes(CallPAL);
		    if (cast<CallInst>(oldCall)->isTailCall()) {
			cast<CallInst>(newCall)->setTailCall();
		    }
		    if (MDNode *tbaa = oldCall->getMetadata(LLVMContext::MD_tbaa)) {
			newCall->setMetadata(LLVMContext::MD_tbaa, tbaa);
		    }
		    if (MDNode *tbaaStruct = oldCall->getMetadata(LLVMContext::MD_tbaa_struct)) {
			newCall->setMetadata(LLVMContext::MD_tbaa_struct, tbaaStruct);
		    }
		    if (debugPassInsn) {
			errs () << "Old Instruction : " << *oldCall << "\n";
			errs () << "New Instruction : " << *newCall << "\n";
		    }
		    VM[oldCall] = newCall;
		    deletedInsn.push_back(oldCall);		
	
		} else {
		    RemapInstruction(targetInsn, VM, RF_IgnoreMissingEntries);
		}
		break;
	    }
	    default:
		RemapInstruction(targetInsn, VM, RF_IgnoreMissingEntries);
		if (debugPassInsn) {
		    errs () << "New Instruction: " << *targetInsn << "\n";
		}
		break;
	    }
	}
	
	void localityOptimization(Module &M, Function* F) {
	    // Don't do anything if there is no body.
	    // Does nothing for special functions since they have no body.
	    if( F->begin() == F->end() ) return;
	    
	    // For Debug
	    if (debugThisFn[0] && F->getName() == debugThisFn) {
		// generate F->getName().before.ll 
		dumpFunction(F, "before");
		debugPassInsn = true;
	    }
	    
	    // Allocate 
	    LocalArrayInfo *LocalArraysGVN = new LocalArrayInfo();
	    LocalArrayInfo *LocalArraysDecl = new LocalArrayInfo();
	    
	    // Create IGraph
	    // Inspect all instructions and construt IGraph. Each node of IGraph contains a densemap that map that is one-to-one mapping of each operand into a specific address space (either 100 or 0).
	    // If an instruction is enclosed by a local statement, set the locality level of each operand to 0.
	    // Currently, we are assuming that gf.addr function calls correspond to Chapel's local statements, but this is not always true because gf.addr is also used to extract a local pointer from a wide pointer. We work on this later pass using NonLocals)       
	    IGraph *G = new IGraph(F->getName());
	    G->construct(F, info);
	    
	    // Perform a reduced version of GVN 
	    ValueTable *VN = createValueTable(F);

	    // Input  : VN, G
	    // Output : LocalArraysGVN, LocalArrayDecl
	    salvageChapelArrayAccess(F, VN, LocalArraysGVN, LocalArraysDecl);

	    // Dump analysis results
	    if (debugThisFn[0] && F->getName() == debugThisFn) {
		VN->dump();
		// For Graphviz
		G->dumpDOT();
		errs () << "\n[Local Array GVN]\n";
		LocalArraysGVN->dump();
		errs () << "[Local Array Decl]\n";
		LocalArraysDecl->dump();
           }
	    
	    // Process each instruction
	    // try to convert load/store/getelementptr with addrspace(100) to addrspace(0) with using IGraph
	    SmallVector<Instruction*, 16> deletedInsn;
	    ValueToValueMapTy ValueMap;
	    for (inst_iterator II = inst_begin(F), IE = inst_end(F); II != IE; ++II) {
		Instruction *insn = &*II;
		processInstruction(insn, deletedInsn, ValueMap, VN, M, G, LocalArraysGVN, LocalArraysDecl);
	    }
	    for (unsigned int i = 0; i < deletedInsn.size(); i++) {
		Instruction *insn = deletedInsn[i];
		insn->removeFromParent();
		insn->setName("");
		insn->dropAllReferences();
	    }

	    // Cleanup
	    // TODO delete
	    ValueMap.clear();
	    deletedInsn.clear();

	    // For Debug
	    if (debugThisFn[0] && F->getName() == debugThisFn) {
		// generate F->getName().before.ll 
		dumpFunction(F, "after");
	    }	
    	}
	
	/*
	  Locality Optimization Pass:
	  
	  This pass tries to replace address space 100 pointer with address space 0 pointer.
	  1. Local Statement (by users)
	  2. Locale local array declaration (by users but not explicitly expressed)
	*/

	virtual bool runOnModule(Module &M) {
	    bool madeInfo = false;
      
	    // Normally we expect a user of this optimization to have
	    // already produced an info object with the important
	    // information, but if not we set some defaults here so
	    // that tests can be created and bugpoint can be run.
	    if( !info ) {
		errs() << "Warning: GlobalToWide using default configuration\n";
		info = new GlobalToWideInfo();
		madeInfo = true;
		info->globalSpace = 100;
		info->wideSpace = 101;
		info->localeIdType = M.getTypeByName("struct.c_localeid_t");
		if( ! info->localeIdType ) {
		    StructType* t = StructType::create(M.getContext(), "struct.c_localeid_t");
		    t->setBody(Type::getInt32Ty(M.getContext()), Type::getInt32Ty(M.getContext()), NULL);
		    info->localeIdType = t;
		}
		info->nodeIdType = Type::getInt32Ty(M.getContext());

		// Now go identify special functions in the module by name.
		for (Module::iterator next_func = M.begin(); next_func!= M.end(); )
		{
		    Function *F = &*next_func;
		    ++next_func;

		    FunctionType* FT = F->getFunctionType();

		    // This may look like a crazy amount of checking, but we
		    // need to do it in order to have bugpoint work with this
		    // optimization, since it will basically try different ways
		    // of corrupting the input.
		    if( F->getName().startswith(GLOBAL_FN_GLOBAL_ADDR) &&
			FT->getNumParams() == 1 &&
			FT->getReturnType()->isPointerTy() &&
			FT->getReturnType()->getPointerAddressSpace() == 0 &&
			containsGlobalPointers(info, FT->getParamType(0)) ) {
			Type* gType = FT->getParamType(0);
			GlobalPointerInfo & r = info->gTypes[gType];
			r.addrFn = F;
			info->specialFunctions.insert(F);
		    } else if( F->getName().startswith(GLOBAL_FN_GLOBAL_LOCID) &&
			       FT->getNumParams() == 1 &&
			       FT->getReturnType() == info->localeIdType &&
			       containsGlobalPointers(info, FT->getParamType(0)) ) {
			Type* gType = FT->getParamType(0);
			GlobalPointerInfo & r = info->gTypes[gType];
			r.locFn = F;
			info->specialFunctions.insert(F);
		    } else if( F->getName().startswith(GLOBAL_FN_GLOBAL_NODEID) &&
			       FT->getNumParams() == 1 &&
			       FT->getReturnType() == info->nodeIdType &&
			       containsGlobalPointers(info, FT->getParamType(0)) ) {
			Type* gType = FT->getParamType(0);
			GlobalPointerInfo & r = info->gTypes[gType];
			r.nodeFn = F;
			info->specialFunctions.insert(F);
		    } else if( F->getName().startswith(GLOBAL_FN_GLOBAL_MAKE) &&
			       FT->getNumParams() == 2 &&
			       FT->getParamType(0) == info->localeIdType &&
			       FT->getParamType(1)->isPointerTy() &&
			       FT->getParamType(1)->getPointerAddressSpace() == 0 &&
			       containsGlobalPointers(info, FT->getReturnType()) ) {
			Type* gType = FT->getReturnType();
			GlobalPointerInfo & r = info->gTypes[gType];
			r.makeFn = F;
			info->specialFunctions.insert(F);
		    } else if( F->getName().startswith(GLOBAL_FN_GLOBAL_TO_WIDE) &&
			       FT->getNumParams() == 1 &&
			       containsGlobalPointers(info, FT->getParamType(0)) ) {
			Type* gType = FT->getParamType(0);
			GlobalPointerInfo & r = info->gTypes[gType];
			r.globalToWideFn = F;
			info->specialFunctions.insert(F);
		    } else if( F->getName().startswith(GLOBAL_FN_WIDE_TO_GLOBAL) &&
			       FT->getNumParams() == 1 &&
			       containsGlobalPointers(info, FT->getReturnType()) ) {
			Type* gType = FT->getReturnType();
			GlobalPointerInfo & r = info->gTypes[gType];
			r.wideToGlobalFn = F;
			info->specialFunctions.insert(F);
		    }
		}
	    }

	    assert(info->globalSpace > 0);
	    assert(info->localeIdType);
	    assert(info->nodeIdType);

	    // Wide pointer address space must differ from the local one...
	    assert(info->globalSpace != 0);
	    assert(info->wideSpace != 0);
	    assert(info->localeIdType != 0);
	    assert(info->nodeIdType != 0);
	    
	    // Note : current implementation is not inter-procedural 
	    for(Module::iterator func = M.begin(); func!= M.end(); func++) {
		Function *F = &*func;
		if (F->getName().startswith(".")) {
		    continue; // skip special functions
		}
		localityOptimization(M, F);
	    }

	    // After it all, put the target info back.
	    if( !madeInfo ) M.setDataLayout(layoutAfterwards);
	    if( madeInfo ) delete info;
	    
	    return true;
	}
  
	
	void salvageChapelArrayAccess(Function *F, ValueTable *VN, LocalArrayInfo *LocalArraysGVN, LocalArrayInfo *LocalArraysDecl) {
	    /* Skip this if there is a branch instruction. */
	    /* (TODO) Integrate salvageChapelArrayAccess into IGraph later*/
	    for (inst_iterator IS = inst_begin(F), IE = inst_end(F); IS != IE; IS++) {
		Instruction *targetInsn = &*IS;
		TerminatorInst *branch = dyn_cast<TerminatorInst>(targetInsn);
		if (branch) {
		    ReturnInst *RI = dyn_cast<ReturnInst>(targetInsn);
		    if (!RI) {
			branch->dump();
			return;
		    }
		}		
	    }

	    for (inst_iterator IS = inst_begin(F), IE = inst_end(F); IS != IE; IS++) {
		Instruction *targetInsn = &*IS;
		switch (targetInsn->getOpcode()) {
		case Instruction::Load:
		case Instruction::Store:
		{
		    // search array access enclosed by local statement
		    analyzeLoadStoreInsn(targetInsn, F, VN, LocalArraysGVN);
		    break;
		}
		case Instruction::Call:
		{
		    // search array construction
		    analyzeCallInsn(targetInsn, VN, LocalArraysDecl);
		    break;
		}
		default:
		    ; /* do nothing */		    
		}		
	    }
	}

	int analyzeArrayAccessOffsets(Instruction *getOffsetGEP) {
	    int ret = -1;
	    if (getOffsetGEP) {
		errs () << "Offset : \n";
		errs () << *getOffsetGEP << "\n";
		Instruction *offsetInsn = dyn_cast<Instruction>(getOffsetGEP->getOperand(1));
		if (offsetInsn) {
		    switch(offsetInsn->getOpcode()) {
		    case Instruction::Load: {			
			ret = 0;
			break;
		    }
		    case Instruction::Shl: {
			Constant *op1 = dyn_cast<Constant>(offsetInsn->getOperand(1));
			if (op1) {
			    ret = 1 << (int)(op1->getUniqueInteger().roundToDouble());
			} else {
			    ret = -1; 
			}
			break;
		    }
		    case Instruction::Mul: {
			Constant *op1 = dyn_cast<Constant>(offsetInsn->getOperand(1));
			if (op1) {
			    ret = (int)(op1->getUniqueInteger().roundToDouble());
			} else {
			    ret = -1;
			}						
		    }
		    }
		}
		
	    }	    
	    return ret;
	}	    
	
	void analyzeLoadStoreInsn(Instruction *I, Function *F, ValueTable *VN, LocalArrayInfo *LocalArrays) {
	    if (isArrayAccessLoadOrStore(I, info->globalSpace)) {
                // for each store/load instruction that involves addrspace 100 and is supposed to be array access.
		if (debugPassInsn) {
		    errs () << *I << " is supposed to be array access\n";
		}
		GetElementPtrInst *gep1 = findGEP08FromMemOp(I);
		if (gep1 == NULL) return;
		for (inst_iterator IS2 = inst_begin(F), IE2 = inst_end(F); IS2 != IE2; IS2++) {
		    Instruction *I2 = &*IS2;
		    // search load/store instruction that is supposed to be local array access.
		    if (I != I2 && isArrayAccessLoadOrStore(I2, 0)) {
			// I  is load/store addrspace(100) ptr and supposed to be array access at this point
			// I2 is load/store addrspace(0)   ptr and supposed to be array access at this point
			GetElementPtrInst *gep2 = findGEP08FromMemOp(I2);
			if (gep2 == NULL) continue;
			if (VN->sameExpressions(gep1, gep2)) {
			    errs () << "[GVN worked!]\n";
			    errs () << "\t Local Access :\n";
			    errs () << "\t\t Load/Store w/ addrspace(0)   : " << *I2 << "\n";			    
			    errs () << "\t\t Array Ptr  w/ addrspace(0)   : " << *gep2 << "\n";
			    errs () << "\t Possibly Remote Access :\n";
			    errs () << "\t\t Load/Store w/ addrspace(100) : " << *I << "\n";			    
			    errs () << "\t\t Array Ptr  w/ addrspace(100) : " << *gep1 << "\n";			    
			    // mark  
			    Value *localArray = gep1->getPointerOperand();
			    LocalArrayEntry *li = LocalArrays->getEntryByValue(localArray);
			    // Analyze Offset
			    // FIXME: this is only for store instruction! (I2->getOperand(0) if it's a load instruction)
			    int offset = analyzeArrayAccessOffsets(dyn_cast<Instruction>(I2->getOperand(1)));
			    if (offset != -1) {
				//
				if (!li) {
				    li = new LocalArrayEntry(localArray, false);
				    li->addLocalOffset(offset);
				    LocalArrays->add(li);
				} else {
				    li->addLocalOffset(offset);
				}
			    }
			}
		    }
		}
	    }
	}

	// check if construct_DefaultRectangularArr is in this function
	void analyzeCallInsn(Instruction *I, ValueTable *VN, LocalArrayInfo *LocalArrays) {
	    if (isa<CallInst>(I)) {
		CallInst *callInsn1 = cast<CallInst>(I);
		Function *calledFunc1 = callInsn1->getCalledFunction();
		// gf.make. 		
		if (calledFunc1 == NULL) {
		    return;
		}
		StringRef funcName = calledFunc1->getName();
		if (funcName.startswith(".gf.make")) { 
		    Value* v = callInsn1->getArgOperand(1);
		    if (isa<CallInst>(v)) {
			CallInst *callInsn2 = cast<CallInst>(v);
			Function *calledFunc2 = callInsn2->getCalledFunction();
			if (calledFunc2 && calledFunc2->getName().startswith("_construct_DefaultRectangularArr")) {
			    LocalArrayEntry *li = new LocalArrayEntry(I, true);
			    LocalArrays->add(li);
			} 
		    }
		} else if (funcName.startswith("chpl__convertRuntimeTypeToValue")) { //
		    Value* v = callInsn1->getArgOperand(1);
		    for (User *U : v->users()) {
			Value *UI = U;
			if (isa<LoadInst>(*UI)) {
			    LoadInst *l = cast<LoadInst>(UI);
			    if (l->getPointerOperand() == v) {
				LocalArrayEntry *li = new LocalArrayEntry(UI, true);
				LocalArrays->add(li);
				// support chpl___ASSIGN
				for (User *LU: l->users()) {
				    Value *LUI = LU;
				    if (isa<CallInst>(LUI)) {
					CallInst *callInsn2 = cast<CallInst>(LUI);
					Function *calledFunc2 = callInsn2->getCalledFunction();
					if (calledFunc2 && calledFunc2->getName().startswith("chpl___ASSIGN_")
					    && UI == callInsn2->getArgOperand(0)) {
					    LocalArrayEntry *li2 = new LocalArrayEntry(LUI, true);
					    LocalArrays->add(li2);
					}
				    }
				}
			    } 			    
			}
		    }
		} else if (funcName.startswith("chpl__buildDomainExpr")) {
		    Value* v = callInsn1->getOperand(1);		    
		    for (User *U : v->users()) {
			Value *UI = U;
			if (isa<LoadInst>(UI)) {
			    LoadInst *l = cast<LoadInst>(UI);
			    if (l->getPointerOperand() == v) {
				LocalArrayEntry *li = new LocalArrayEntry(UI, true);
				LocalArrays->add(li);
			    }
			}
		    }
		}
	    }    
	}

    	void searchGEP08Inst(vector<GetElementPtrInst*> &list, vector<Instruction*> &visited, Instruction* I) {
	    // see if this Instruction is already visited
	    if (find(visited.begin(), visited.end(), I)  != visited.end()) {
		return;
	    }		
	    visited.push_back(I);
	    if (debugPassInsn) {
		errs () << "Parent Insn : " << *I <<"\n";
	    }
	    // Check if this intruction is GEP
	    GetElementPtrInst *gepInst = dyn_cast<GetElementPtrInst>(I);	    
	    if (gepInst && gepInst->getNumIndices() == 2) {
		if (debugPassInsn) {
		    errs () << "Candidate GEP : " << *gepInst << "\n";
		}
		Constant *op1 = dyn_cast<Constant>(gepInst->getOperand(1));
		Constant *op2 = dyn_cast<Constant>(gepInst->getOperand(2));
		if (op1 != NULL && op2 != NULL
		    && op1->getUniqueInteger() == 0 && op2->getUniqueInteger() == 8) {
		    // add a candidate GEP to list
		    if (find(list.begin(),
			     list.end(),
			     gepInst) == list.end()) {
			list.push_back(gepInst);
		    }
		}
	    } else {
		for (unsigned int i=0; i < I->getNumOperands(); i++) {
		    Value *op = I->getOperand(i);
		    Instruction *insn = dyn_cast<Instruction>(op);
		    if (insn) {
			searchGEP08Inst(list, visited, insn);
		    }
		}
	    }
	}

	GetElementPtrInst* findGEP08FromMemOp(Instruction *I) {
	    Value *op = NULL;
	    if (isa<StoreInst>(I)) {
		StoreInst* s = cast<StoreInst>(I);
		op = s->getPointerOperand();
	    } else if (isa<LoadInst>(I)) {
		LoadInst* l = cast<LoadInst>(I);
		op = l->getPointerOperand();
	    } else {
		return NULL;
	    }
	    Instruction *insn;
	    if (!isa<Instruction>(op)) {
		return NULL;
	    } else {
		insn = cast<Instruction>(op);
	    }
	    vector<GetElementPtrInst*> list;
	    vector<Instruction*> visited;
	    for (unsigned int i = 0; i < insn->getNumOperands(); i++) {
		Value *op2 = insn->getOperand(i);
		Instruction *insn2 = dyn_cast<Instruction>(op2);
		if (insn2) {
		    searchGEP08Inst(list, visited, insn2);
		}
	    }
	    if (list.size() == 0) {
		return NULL;
	    } else {
		if (list.size() != 1) {
		    errs () << "Warning : indirect access detected\n";
		}
		return list[0];
	    }
	}

	bool isArrayAccessGEP(GetElementPtrInst* gep) {
	    if (gep != NULL) {
		Type* t = gep->getOperand(0)->getType();
		if (t->isPointerTy()) {
		    Type* t2 = t->getPointerElementType();
		    if (isa<StructType>(t2)) {
			if (t2->getStructName().startswith("chpl__class")) {
			    return false;
			}		    
		    } else {
			errs () << "not struct\n";
		    }
		}
	    } else {
		return false;
	    }		
	    return true;
	}

	bool isArrayAccessLoadOrStore(Instruction *I, unsigned addrSpace) {
	    if (isa<StoreInst>(I)) {
		StoreInst* s = cast<StoreInst>(I);
		if (s->getPointerAddressSpace() != addrSpace) {
		    return false;
		}
		GetElementPtrInst* gep = findGEP08FromMemOp(s);
		return isArrayAccessGEP(gep);
	    } else if (isa<LoadInst>(I)) {
		LoadInst* l = cast<LoadInst>(I);
		if (l->getPointerAddressSpace() != addrSpace) {
		    return false;
		}
		errs () << "Load + 100 : " << *I << "\n";
		GetElementPtrInst* gep = findGEP08FromMemOp(l);
		return isArrayAccessGEP(gep);
	    } else {
		return false;
	    }
	}
    };
}

char LocalityOpt::ID = 0;
static RegisterPass<LocalityOpt> X("locality-opt", "Locality Optimization Pass");

ModulePass *createLocalityOpt(GlobalToWideInfo* info, std::string setlayout) {
    return new LocalityOpt(info, setlayout);
}
#endif

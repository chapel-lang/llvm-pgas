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
// SSA Value Graph for Locality Inference
//===----------------------------------------------------------------------===//

#include "IGraph.h"

#include <vector>
#include <string>
#include <fstream>


#if HAVE_LLVM_VER >= 35
#include "llvm/IR/InstIterator.h"
#else
#include "llvm/Support/InstIterator.h"
#endif

using namespace std;
using namespace llvm;

Value* IGraph::getOperandIfLocalStmt(Instruction *insn) {
    CallInst *call = dyn_cast<CallInst>(insn);
    if (call) {
	Function* f = call->getCalledFunction();
	if (f != NULL) {
	    // calling @.gf.addr and then doing load and store => local statement
	    if (f->getName().startswith(".gf.addr")) {
		for (User *U : call->getArgOperand(0)->users()) {
		    Value *UI = U;
		    if (isa<LoadInst>(*UI) || isa<StoreInst>(*UI)) {
			return call->getArgOperand(0);
		    }
		}
	    }
	}
    }
    return NULL;
}

void IGraph::construct(Function *F, GlobalToWideInfo *info) {

    if (debug) {
	errs () << "[Inequality Graph Construction for " << F->getName() << "]\n";
    }

    /* First create an entry node */
    Node *entry = new Node(NODE_ENTRY, NULL, NULL, 0, 0);
    this->entry = entry;
    this->addNode(entry);
    
    /* 1. collect addrspace 100 pointers that is used in the next step. */
    /*    1. construct a set of addrspace 100 pointers. */
    /*    2. construct a list of blocks that def/use the pointer. */
    SmallVector<Value*, 128> possiblyRemotePtrs;
    SmallVector<Value*, 128> possiblyRemoteArgs;
    // analyze arguments
    for (Function::arg_iterator I = F->arg_begin(), E = F->arg_end(); I!=E; ++I) {
	Value *arg = I;	
	if (arg->getType()->isPointerTy() && arg->getType()->getPointerAddressSpace() == info->globalSpace) {
	    if (find(possiblyRemotePtrs.begin(),
		     possiblyRemotePtrs.end(),
		     arg) == possiblyRemotePtrs.end()) {
		possiblyRemotePtrs.push_back(arg);
	    }
	    if (find(possiblyRemoteArgs.begin(),
		     possiblyRemoteArgs.end(),
		     arg) == possiblyRemoteArgs.end()) {
		possiblyRemoteArgs.push_back(arg);
	    }
	}
    }

    // analyze instructions
    DenseMap<Instruction*, std::tuple<NodeKind, Value*, Instruction*, int>> NodeCandidates;
    for (inst_iterator II = inst_begin(F), IE = inst_end(F); II != IE; ++II) {
	Instruction *insn = &*II;
	bool needToWork = false;
	// 
	NodeKind kind = NODE_NONE;
	Value *ptrOp = NULL;
	int addrspace = 100;
	// 
	switch (insn->getOpcode()) {
	case Instruction::Load: {
	    LoadInst *load = cast<LoadInst>(insn);
	    if(load->getPointerAddressSpace() == info->globalSpace) {
		needToWork = true;
		kind = NODE_USE;
		ptrOp = load->getPointerOperand();
		addrspace = 100;
	    }
	    break;
	}
	case Instruction::Call: {
	    ptrOp = getOperandIfLocalStmt(insn);
	    if (ptrOp) {
		needToWork = true;
		kind = NODE_DEF;
		addrspace = 0;
	    }
	}
	// TODO: store/getelementptr insn	
	}
	if (needToWork) {
	    // collect possibly remote pointers
	    if (find(possiblyRemotePtrs.begin(),
		     possiblyRemotePtrs.end(),
		     ptrOp) == possiblyRemotePtrs.end()) {
		possiblyRemotePtrs.push_back(ptrOp);
	    }
	    // store detailed information used in node construction.	    
	    NodeCandidates[insn] = std::make_tuple(kind, ptrOp, insn, addrspace);
	}
    }
	
    /* 2. for each pointer do the following. */
    /* */
    for (SmallVector<Value*, 128>::iterator I = possiblyRemotePtrs.begin(),
	     E = possiblyRemotePtrs.end(); I != E; I++) {
	Value* val = *I;
	if (debug) {
	    errs () << "Working on :" << *val << "\n";	    
	}

	DenseMap<BasicBlock*, std::pair<Node*, Node*>> BBInfo;
	bool firstOccurrence = true;
	// Create Intra-block edge
	for (Function::iterator BI = F->begin(), BE = F->end(); BI != BE; BI++) {
	    BasicBlock* BB = BI;
	    // remember first and last node in BB so we can create edges between blocks.
	    Node *firstNodeInBB = NULL;
	    Node *lastNodeInBB = NULL;
	    
	    // create node for arguments
	    if (BI == F->begin()) {
		if (find(possiblyRemoteArgs.begin(),
			 possiblyRemoteArgs.end(),
			 val) != possiblyRemoteArgs.end()) {				
		    Node *n = new Node(NODE_DEF, val, NULL, 0, 100);
		    this->addNode(n);
		    firstNodeInBB = n;
		    lastNodeInBB = n;				    
		    if (firstOccurrence) {
			entry->addChild(n);
		    }
		}		
	    }

	    // For each instruction
	    // Create a node if an instruction contains possibly-remote access
	    for (BasicBlock::iterator I = BB->begin(), E = BB->end(); I != E; I++) {
		// add edge if needed
		Instruction *insn = &*I;
		if (NodeCandidates.find(insn) != NodeCandidates.end()) {
		    std::tuple<NodeKind, Value*, Instruction*, int> &info = NodeCandidates[insn];
		    // create a new node.
		    Node *n = new Node(std::get<0>(info),  // Kind
				       std::get<1>(info),  // Value
				       std::get<2>(info),  // Insn
				       0,                  // Version (0 for now)
				       std::get<3>(info)); // Locality (either 0 or 100)
		    // register the created node to the Graph.
		    this->addNode(n);
		    if (!firstNodeInBB) {
			// First node in the current BB.
			firstNodeInBB = n;			
		    }
		    if (lastNodeInBB) {
			// There exists a predecessor node here.
			// append the created node to a predecessor node.
			lastNodeInBB->addChild(n);
			n->addParents(lastNodeInBB);
		    }
		    lastNodeInBB = n;
		}		
	    }
	    //
	    BBInfo[BB] = std::make_pair(firstNodeInBB, lastNodeInBB);	    
	}
	
	// Inter-block edge
	for (Function::iterator BI = F->begin(), BE = F->end(); BI != BE; BI++) {
	    BasicBlock* BB = BI;
	    std::pair<Node*, Node*> &SrcBBinfo = BBInfo[BB];
	    const TerminatorInst *TInst = BB->getTerminator();
	    // get the last node of the current BB
	    Node* srcNode = std::get<1>(SrcBBinfo);
	    if (!srcNode) {
		continue;
	    }
	    // Succ	    
	    for (unsigned I = 0, NSucc = TInst->getNumSuccessors(); I < NSucc; I++) {
		BasicBlock *Succ = TInst->getSuccessor(I);
		std::pair<Node*, Node*> &DstBBinfo = BBInfo[Succ];
		Node* dstNode = std::get<0>(DstBBinfo);
		if (dstNode) {
		    srcNode->addChild(dstNode);
		    dstNode->addParents(srcNode);
		}
	    }		    
	}
	// Dominator Tree Computation
        // Dominator Frontier Computation
	
	// phi-insertion
	
	// renaming
	
    }       
}

void IGraph::dumpDOT() {
    std::string Filename = "ig." + this->getName().str() + ".dot";
#if HAVE_LLVM_VER >= 35
    std::error_code EC;
    raw_fd_ostream File(Filename.c_str(), EC, sys::fs::F_Text);
    
    if (!EC) {
	WriteGraph(File, (const IGraph*)this, false, "Habanero");
    } else {
	errs() << "Dump IGraph : error: "<< EC.message() << "\n";

    }
#else
    std::string ErrorInfo;
    raw_fd_ostream File(Filename.c_str(), ErrorInfo);
    
    if (ErrorInfo.empty()) {
	WriteGraph(File, (const IGraph*)this, false, "Habanero");
    } else {
	errs() << "  error opening file for writing!";
    }
#endif    
}       

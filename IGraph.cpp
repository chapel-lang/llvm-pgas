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
#include <sstream>

#if HAVE_LLVM_VER >= 35
#include "llvm/IR/InstIterator.h"
#else
#include "llvm/Support/InstIterator.h"
#endif

using namespace std;
using namespace llvm;

void Node::printAsOperandInternal(raw_ostream &o, Value* value) const {
#if HAVE_LLVM_VER >= 35
    value->printAsOperand(o, false);
#else	    
    WriteAsOperand(o, value, false);
#endif
}

void Node::printAsOperand(raw_ostream &o, bool PrettyPrint) const {
    // print Node Information
    if (PrettyPrint) {
	switch (this->getKind()) {
	case NODE_ENTRY:
	    o << "entry";
	    break;
	case NODE_DEF:
	    printAsOperandInternal(o, value);
	    o << "_" << this->getVersion() << " = " << this->getLocality();
	    break;
	case NODE_USE:
	    o << "... = ";
	    printAsOperandInternal(o, value);
	    o << "_" << this->getVersion();
	    break;
	case NODE_PHI:
	    printAsOperandInternal(o, value);
	    o << "_" << this->getVersion();
		
	    o << " = phi(";
	    for (const_iterator I = this->parents_begin(),
		     E = this->parents_end(); I != E; I++) {
		Node *n = *I;
		n->printAsOperand(o, false);
		if (I+1 != E) {
		    o << ", ";
		}
	    }
	    o << ")";
	    break;
	case NODE_PI:
	    break;
	default:
	    assert(0 && "Inequality Graph Node Type should not be NODE_NONE");
	}	    
	o << "\n" << this->getPostOrderNumber();
#ifdef DEBUG
	o << "\n" << "Parents (";
	for (const_iterator I = parents_begin(), E = parents_end(); I != E; I++) {
	    Node *n = *I;
	    o << n->getPostOrderNumber();
	    if (I+1 != E) {
		o << ", ";
	    }
	}
	o << ")";
	o << "\n" << "Children (";
	for (const_iterator I = children_begin(), E = children_end(); I != E; I++) {
	    Node *n = *I;	       
	    o << n->getPostOrderNumber(); 
	    if (I+1 != E) {
		o << ", ";
	    }
	}
	o << ")";
#endif
    } else {
	NodeKind kind = this->getKind();
	if (kind == NODE_DEF || kind == NODE_USE) {
	    printAsOperandInternal(o, value);
	    o << "_" << this->getVersion();
	}
    }

}    

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

IGraph::InsnToNodeMapType IGraph::analyzeDefUseOfLocality(Function *F, GlobalToWideInfo *info) {
    /* 1. collect addrspace 100 pointers that is used in the next step. */
    /*    1. construct a set of addrspace 100 pointers. */
    /*    2. construct a list of blocks that def/use the pointer. */
    // analyze arguments
    if (debug) {
	errs () << "\t analyzing Def/Use of Locality\n";
    }
    for (Function::arg_iterator I = F->arg_begin(), E = F->arg_end(); I!=E; ++I) {
	Value *arg = I;	
	if (arg->getType()->isPointerTy()
	    && arg->getType()->getPointerAddressSpace() == info->globalSpace) {
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
    InsnToNodeMapType NodeCandidates;
    for (inst_iterator II = inst_begin(F), IE = inst_end(F); II != IE; ++II) {
	Instruction *insn = &*II;
	bool needToWork = false;
	// 
	Node::NodeKind kind = Node::NODE_NONE;
	Value *ptrOp = NULL;
	int addrspace = 100;
	// 
	switch (insn->getOpcode()) {
	case Instruction::Load: {
	    LoadInst *load = cast<LoadInst>(insn);
	    if(load->getPointerAddressSpace() == info->globalSpace) {
		needToWork = true;
		kind = Node::NODE_USE;
		ptrOp = load->getPointerOperand();
		addrspace = 100;
	    }
	    break;
	}
	case Instruction::Call: {
	    ptrOp = getOperandIfLocalStmt(insn);
	    if (ptrOp) {
		needToWork = true;
		kind = Node::NODE_DEF;
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
    return NodeCandidates;
}    

void IGraph::buildGraph(Function *F, InsnToNodeMapType &NodeCandidates) {
    if (debug) {
	errs () << "\t buidling an initial graph\n";
    }
    // Build a graph based on NodeCandidates construted in the previous phase (namely analyzeDefUseOfLocality)
    for (PossiblyRemoteArrayType::iterator I = possiblyRemotePtrs.begin(),
	     E = possiblyRemotePtrs.end(); I != E; I++) {
	Value* val = *I;
	if (debug) {
	    errs () << "\t\tWorking on :" << *val << "\n";	    
	}
	// to record the first and last node in BB
	DenseMap<BasicBlock*, std::pair<Node*, Node*>> BBInfo;
	// 
	bool firstOccurrence = true;
	// Create Intra-block edge
	for (Function::iterator BI = F->begin(), BE = F->end(); BI != BE; BI++) {
	    BasicBlock* BB = BI;
	    // remember first and last node in BB so we can create edges between blocks.
	    Node *firstNodeInBB = NULL;
	    Node *lastNodeInBB = NULL;
	    bool nodeAdded = false;
	    
	    // For the first block, create node for arguments
	    if (BI == F->begin()) {
		if (find(possiblyRemoteArgs.begin(),
			 possiblyRemoteArgs.end(),
			 val) != possiblyRemoteArgs.end()) {
		    // an argument that involoves address space 100 is DEF node
		    Node *n = new Node(Node::NODE_DEF, val, NULL, 0, 100);
		    this->addNode(n);
		    nodeAdded = true;
		    firstNodeInBB = n;
		    lastNodeInBB = n;				    
		    if (firstOccurrence) {
			entry->addChild(n);
			n->addParent(entry);
			firstOccurrence = false;
		    }
		}		
	    }

	    // For each instruction
	    // Create a node if an instruction contains possibly-remote access

	    for (BasicBlock::iterator I = BB->begin(), E = BB->end(); I != E; I++) {
		// add edge if needed
		Instruction *insn = &*I;
		if (NodeCandidates.find(insn) != NodeCandidates.end()) {
		    std::tuple<Node::NodeKind, Value*, Instruction*, int> &info = NodeCandidates[insn];
		    // create a new node.
		    Node *n = new Node(std::get<0>(info),  // Kind
				       std::get<1>(info),  // Value
				       std::get<2>(info),  // Insn
				       0,                  // Version (0 for now)
				       std::get<3>(info)); // Locality (either 0 or 100)
		    // register the created node to the Graph.
		    this->addNode(n);
		    nodeAdded = true;
		    if (firstOccurrence) {
			entry->addChild(n);
			n->addParent(entry);
			firstOccurrence = false;
		    }
		    if (!firstNodeInBB) {
			// First node in the current BB.
			firstNodeInBB = n;			
		    }
		    if (lastNodeInBB) {
			// There exists a predecessor node here.
			// append the created node to a predecessor node.
			lastNodeInBB->addChild(n);
			n->addParent(lastNodeInBB);
		    }
		    lastNodeInBB = n;
		}		
	    } // for each instruction
	    if (nodeAdded) {
		BBInfo[BB] = std::make_pair(firstNodeInBB, lastNodeInBB);
	    } else {
		Node *dummyUSENode = new Node(Node::NODE_USE, val, NULL, 0, 100);
		this->addNode(dummyUSENode);
		BBInfo[BB] = std::make_pair(dummyUSENode, dummyUSENode);
	    }
	} // for each block

	// Add inter-block edges
	// For each block in Function
	for (Function::iterator BI = F->begin(),
		 BE = F->end(); BI != BE; BI++) {
	    // The current BB
	    BasicBlock* BB = BI;
	    // get the first and last node in this BB
	    std::pair<Node*, Node*> &SrcBBinfo = BBInfo[BB];
	    // get<1> : the last node in this BB
	    Node* srcNode = std::get<1>(SrcBBinfo);
	    const TerminatorInst *TInst = BB->getTerminator();
	    // add edges :
	    // the last node in the current BB -> the first node in succesor BBs
	    for (unsigned I = 0, NSucc = TInst->getNumSuccessors(); I < NSucc; I++) {
		BasicBlock *Succ = TInst->getSuccessor(I);
		std::pair<Node*, Node*> &DstBBinfo = BBInfo[Succ];
		Node* dstNode = std::get<0>(DstBBinfo);
		if (dstNode) {
		    srcNode->addChild(dstNode);
		    dstNode->addParent(srcNode);
		}
	    }		    
	}
    }
}

void IGraph::calculateDTandDF() {
    // Dominator Tree Computation
    setPostOrderNumberWithDFS();
    computeDominatorTree();    
    // Dominator Frontier Computation
    computeDominanceFrontier();
}

void IGraph::setPostOrderNumberWithDFSInternal(Node *node, int &number, Node::NodeElementType &visited) {
    visited.push_back(node);
    
    for (Node::iterator I = node->children_begin(),
	                E = node->children_end();
	 I != E; I++) {
	Node *child = *I;
	if (find(visited.begin(), visited.end(), child) == visited.end()) {
	    setPostOrderNumberWithDFSInternal(child, number, visited);
	}
    }
    if (node->getPostOrderNumber() == -1) {
	node->setPostOrderNumber(number++);
    }
}

void IGraph::setPostOrderNumberWithDFS() {
    if (debug) {
	errs () << "\t setting post order number\n";
    }

    /* 1. Reset Post order number */
    for (IGraph::iterator I = this->begin(),
	     E = this->end(); I != E; I++) {
	Node *n = *I;
	n->resetPostOrderNumber();
    }
    /* 2. set post order number recursively */
    Node *entry = this->getEntry();
    entry->setPostOrderNumber(this->size() - 1);
    int number = 0;
    Node::NodeElementType visited;
    setPostOrderNumberWithDFSInternal(entry, number, visited);
    assert(this->size() - 1  == number);
}

void IGraph::computeDominatorTree() {
    if (debug) {
	errs () << "\t computing dominator tree\n";
    }

    /* initialize the domiantor array */
    for (IGraph::iterator I = this->begin(),
	     E = this->end(); I != E; I++) {
	Node *node = *I;
	if (node == this->getEntry()) {
	    Node::IDominatorTreeType init(this->size(), false);
	    init[node->getPostOrderNumber()] = true;
	    node->setIDom(init);
	    node->setUndefined(false);
	} else {
	    node->setUndefined(true);
	}
    }

    bool Changed = true;
    while (Changed) {
	Changed = false;
	/* in reverse postorder except entry node */
	for (int i = this->size() - 1; i >= 0; i--) {
	    Node * b = this->getNodeByPostOrderNumber(i);
	    if (b == this->getEntry()) continue;
	    /* pick one first processed predecessor  */
	    Node::IDominatorTreeType new_idom(this->size(), false);
	    errs () << "PostOrder(" << i << ")\n";
	    Node *first_pred = NULL;
	    for (IGraph::iterator IPRED = b->parents_begin(),
		     EPRED = b->parents_end(); IPRED != EPRED; IPRED++) {
		Node *node = *IPRED;
		if (!node->getUndefined()) {
		    first_pred = node;
		    break;
		}
	    }
	    assert(first_pred != NULL);
	    new_idom[first_pred->getPostOrderNumber()] = true;
	    for (Node::iterator IPRED = b->parents_begin(),
		     EPRED = b->parents_end(); IPRED != EPRED; IPRED++) {
		Node *p = *IPRED;
		if (p == first_pred) continue;
		if (!p->getUndefined()) {
		    new_idom.reset();
		    int idx = computeIntersect(p, first_pred)->getPostOrderNumber();
		    new_idom[idx] = true;
		}
	    }
	    if (b->getIDom() != new_idom) {
		b->setIDom(new_idom);
		b->setUndefined(false);
		Changed = true;
	    }
	}
    }
    if (debug) {	
	for (IGraph::iterator I = this->begin(), E = this->end(); I != E; I++) {
	    Node *n = *I;
	    errs () << "IDOM(" << n->getPostOrderNumber() << ") : ";
	    Node::IDominatorTreeType b = n->getIDom();
	    for (int i = 0; i < b.size(); i++) {
		if (b[i]) {
		    errs () << i << ", ";
		}
	    }
	    errs () << "\n";
	}
    }
}

Node* IGraph::computeIntersect(Node* b1, Node* b2) {
    Node *finger1 = b1;
    Node *finger2 = b2;
    while (finger1->getPostOrderNumber() != finger2->getPostOrderNumber()) {
	while (finger1->getPostOrderNumber() < finger2->getPostOrderNumber()) {
	    assert(finger1->getIDom().count() == 1);
	    finger1 = this->getNodeByPostOrderNumber(finger1->getIDom().find_first());
	}
	while (finger2->getPostOrderNumber() < finger1->getPostOrderNumber()) {
	    assert(finger2->getIDom().count() == 1);
	    finger2 = this->getNodeByPostOrderNumber(finger2->getIDom().find_first());
	}
    }
    return finger1;
}

void IGraph::computeDominanceFrontier() {
    if (debug) {
	errs () << "\t computing dominance frontier\n";
    }

    /* 1. Reset Dominance Frontier */
    for (IGraph::iterator I = this->begin(), E = this->end(); I != E; I++) {
	Node *b = *I;
	b->resetDominanceFrontier();
    }

    /* 2. Compute Dominacne Frontier */
    for (IGraph::iterator I = this->begin(), E = this->end(); I != E; I++) {
	Node *b = *I;
	if (b->getNumPreds() >= 2) {
	    for (Node::iterator BI = b->parents_begin(),
		     BE = b->parents_end(); BI != BE; BI++) {
		Node *runner = *BI;
		while (runner->getPostOrderNumber() != b->getIDom().find_first()) {
		    runner->addToDominanceFrontier(b);
		    runner = this->getNodeByPostOrderNumber(runner->getIDom().find_first());
		} 
	    }		 
	}
    }

    /* 3. Dump Dominance Frontier if needed */
    if (debug) {
	for (IGraph::iterator I = this->begin(), E = this->end(); I != E; I++) {
	    Node *b = *I;
	    errs () << "DF(" << b->getPostOrderNumber() << ") : "; 
	    for (Node::df_iterator DI = b->df_begin(),
		     DE = b->df_end(); DI != DE; DI++) {
		Node* df = *DI;
		errs () << df->getPostOrderNumber() << ", ";	    
	    }
	    errs () << "\n";
	}
    }

}

void IGraph::performPhiNodeInsertion(bool &Changed) {
    if (debug) {
	errs () << "\t performing phi-node insertion\n";
    }

    SmallVector<Node*, 128> phiAddedNodes; /* set of nodes where phi is added */
    // For each addrspace(100) pointer
    for (PossiblyRemoteArrayType::iterator I = possiblyRemotePtrs.begin(),
	     E = possiblyRemotePtrs.end(); I != E; I++) {
	Value* val = *I;
	SmallVector<Node*, 128> DEFSites;
	// Build a set of nodes that define the current addrspace(100) pointer
	for (IGraph::iterator NI = this->begin(),
		 NE = this->end(); NI != NE; NI++) {
	    Node *node = *NI;
	    if (node->getKind() == Node::NODE_DEF && node->getValue() == val) {
		DEFSites.push_back(node);
	    }
	}
	// For each node that defines the current addrspace(100) pointer
	while (!DEFSites.empty()) {
	    Node* DEFNode = DEFSites[0];	    
	    DEFSites.erase(DEFSites.begin());
	    // For each dominance frontier of the current node
	    for (Node::df_iterator DI = DEFNode->df_begin(),
		     DE = DEFNode->df_end(); DI != DE; DI++) {
		Node *DFofDEF = *DI;
		// skip if a phi-node is already inserted
		if (find(phiAddedNodes.begin(),
			 phiAddedNodes.end(),
			 DFofDEF) == phiAddedNodes.end()) {
		    Node *phiNode = new Node(Node::NODE_PHI, val, NULL, 0, 0);
		    this->addNode(phiNode);
		    // inserting a new phi-node
		    // preserve parents and children of DFofDEF first (TODO functionalize)
		    Node::NodeElementType DFofDEFParents;
		    for (Node::iterator NI = DFofDEF->parents_begin(),
			     NE = DFofDEF->parents_end();
			 NI != NE; NI++) {
			Node *parents = *NI;			
			DFofDEFParents.push_back(parents);
		    }
		    for (Node::iterator NI = DFofDEFParents.begin(),
			     NE = DFofDEFParents.end(); NI != NE; NI++) {
			//
			Node *parents = *NI;
			DFofDEF->eraseFromParent(parents);
			parents->eraseFromChild(DFofDEF);
			parents->addChild(phiNode);
			phiNode->addParent(parents);			
		    }		   
  		    DFofDEF->addParent(phiNode);
		    phiNode->addChild(DFofDEF);
		    phiAddedNodes.push_back(DFofDEF);
		    // phi-node is also DEF node
		    if (find(DEFSites.begin(),
			     DEFSites.end(), DFofDEF) == DEFSites.end()) {
			DEFSites.push_back(DFofDEF);
		    }
		}
	    }
	}
    }
    Changed = phiAddedNodes.size() > 0;
}

void IGraph::generateName(Value *v) {
    int i = renamingCounters[v];
    renamingStacks[v].push(i);
    renamingCounters[v] = i + 1;    
}

void IGraph::performRenamingInternal(Node *n, Node::NodeElementType &visited) {
    if (find(visited.begin(), visited.end(), n) != visited.end()) {
	return;
    } else {
	visited.push_back(n);
    }
    
    switch (n->getKind()) {
    case Node::NODE_PHI:
	generateName(n->getValue());
	n->setVersion(renamingStacks[n->getValue()].top());
	break;
    case Node::NODE_USE:
	n->setVersion(renamingStacks[n->getValue()].top());
	break;
    case Node::NODE_DEF:
	generateName(n->getValue());
	n->setVersion(renamingStacks[n->getValue()].top());
	break;
    default:
	; // do nothing
    }
    //
    for (IGraph::iterator I = this->begin(), E = this->end(); I != E; I++) {
	Node *node = *I;
	// see if the node is a children of n in DT
	if (n != node
	    && n->getPostOrderNumber() == node->getIDom().find_first()) {
	    performRenamingInternal(node, visited);
	}
    }
    if (n->getKind() == Node::NODE_DEF) {
	renamingStacks[n->getValue()].pop();
    }
}

void IGraph::performRenaming() {
    if (debug) {
	errs () << "\t performing renaming\n";
    }

    // 1. initilize counters and stacks
    for (PossiblyRemoteArrayType::iterator I = possiblyRemotePtrs.begin(),
	     E = possiblyRemotePtrs.end(); I != E; I++) {
	Value* v = *I;
	renamingCounters[v] = 0;
	StackType st;
	renamingStacks[v] = st;
    }
    // 2. perform renaming
    Node::NodeElementType visited;    
    Node *entry = this->getEntry();    
    performRenamingInternal(entry, visited);
}

void IGraph::construct(Function *F, GlobalToWideInfo *info) {

    if (debug) {
	errs () << "[Inequality Graph Construction for " << F->getName() << "]\n";
    }

    /* First create an entry node */
    Node *entry = new Node(Node::NODE_ENTRY, NULL, NULL, 0, 0);
    this->entry = entry;
    this->addNode(entry);

    /* 1. Analyze def/use of locality in the function */
    // 1-1 : collect addrspace 100 pointers
    // 1-2 : create an instruction to def/use mapping
    InsnToNodeMapType NodeCandidates = this->analyzeDefUseOfLocality(F, info);

    /* 2. Build an initial IGraph */
    this->buildGraph(F, NodeCandidates);

    /* 3. Build Locality SSA over IGraph for live-range splitting */

    // 3-1. Calculate Dominator Tree and Dominance Frontier 
    this->calculateDTandDF();
    /* 3-2. Insert Phi-nodes using Dominance Frontier */
    bool Changed = false;
    this->performPhiNodeInsertion(Changed);
    /* 3-3. Compute DT again if the shape of the graph is changed */
    if (Changed) {
	this->calculateDTandDF();
    }    
    /* 3-4. Renaming */
    this->performRenaming();
}

void IGraph::dumpDOT() {
    static int version = 0;
    stringstream ss;
    ss << version++;
    std::string Filename = "ig." + this->getName().str() +ss.str() + ".dot";
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

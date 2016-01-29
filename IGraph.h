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

#ifndef _IGRAPH_H_
#define _IGRAPH_H_

#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/GraphTraits.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/DOTGraphTraits.h"
#include "llvm/Support/raw_ostream.h"
#include "llvmUtil.h"

#if HAVE_LLVM_VER >= 35
#else
#include "llvm/Assembly/Writer.h"
#endif

#include <vector>
#include <string>
using namespace std;
using namespace llvm;

class Node {
private:
    StringRef name;
    Value* value;
    vector<Node*> children;
    vector<Node*> parents;
    DenseMap<Value*, unsigned> LLMap;
    unsigned int getAddressSpace(Value *v);
    void initLLMap();

public:
    Node(Value* _value) { value = _value; initLLMap(); };

    // parents
    vector<Node*>::iterator parents_begin() { return parents.begin(); }
    vector<Node*>::iterator parents_end() { return parents.end(); }
    // children
    vector<Node*>::iterator begin() { return children.begin(); }
    vector<Node*>::iterator end() { return children.end(); }
    vector<Node*>::const_iterator begin() const { return children.begin(); }
    vector<Node*>::const_iterator end() const { return children.end(); }

    int getLL(Value* v) const {
	int ll;
	if (LLMap.find(v) != LLMap.end()) { 
		ll = LLMap.find(v)->second;
	} else {
	    ll = -1;
	}
	return ll;
    }
    
    void addParents(Node* parent) {
	parents.push_back(parent);
    }

    void addChild(Node *child) { 
	vector<Node*>::iterator I = find(children.begin(), children.end(), child);
        if( I == children.end() ){
	    children.push_back(child); 
	    child->addParents(this);
	}
    }
    
    StringRef getName() const { return name; }
    Value* getValue() const { return value; }

};

class IGraph {
private:
    StringRef name;
    Node* entry;
    vector<Node*> nodes;
    vector<Node*> getRootNodes() { return nodes; }
public:
    IGraph (StringRef _name) { name = _name; }
    Node* getEntry() const { return entry; }
    StringRef getName() const { return name; }
    
    vector<Node*>::iterator begin() { return nodes.begin(); }
    vector<Node*>::iterator end() { return nodes.end(); }
    vector<Node*>::const_iterator begin() const { return nodes.begin(); }
    vector<Node*>::const_iterator end() const { return nodes.end(); }

    Node* getNodeByValue(const Value* v) { 
	for (vector<Node*>::iterator I = nodes.begin(), E = nodes.end(); I != E; I++) {
	    Node* tmp = *I;
	    if (v == tmp->getValue()) {
		return tmp;
	    }
	}
	return NULL;
    }
    void addNode(Node* n) { nodes.push_back(n); } 

    unsigned size() const { return nodes.size(); }
    void createGraphVizFile(const char* fileName);

    // for GDB
    void dump();
};

namespace llvm {
    template<> struct GraphTraits<Node*> {
	typedef Node NodeType;
	typedef std::vector<Node*>::iterator ChildIteratorType;
	
	static NodeType *getEntryNode(Node *node) { return node; }
	static inline ChildIteratorType child_begin(NodeType *N) { return N->begin(); }
	static inline ChildIteratorType child_end(NodeType *N) { return N->end(); }

    };
    template<> struct GraphTraits<const Node*> {
	typedef const Node NodeType;
	typedef vector<Node*>::const_iterator ChildIteratorType;
	
	static NodeType *getEntryNode(const Node *node) { return node; }
	static inline ChildIteratorType child_begin(const NodeType *N) { return N->begin(); }
	static inline ChildIteratorType child_end(const NodeType *N) { return N->end(); }

    };

    template<> struct GraphTraits<IGraph*> : public GraphTraits<Node*> {
	static NodeType *getEntryNode(IGraph *G) { return G->getEntry(); }
	typedef std::vector<Node*>::iterator nodes_iterator;

	static nodes_iterator nodes_begin(IGraph *G) { return G->begin(); }
	static nodes_iterator nodes_end(IGraph *G) { return G->end(); }
	static unsigned nodes_size(IGraph *G) { return G->size(); }
    };

    template<> struct GraphTraits<const IGraph*> : public GraphTraits<const Node*> {
	static NodeType *getEntryNode(const IGraph *G) { return G->getEntry(); }
	typedef vector<Node*>::const_iterator nodes_iterator;

	static nodes_iterator nodes_begin(const IGraph *G) { return G->begin(); }
	static nodes_iterator nodes_end(const IGraph *G) { return G->end(); }
	static unsigned nodes_size(const IGraph *G) { return G->size(); }
    };
    
    template<> struct DOTGraphTraits<const IGraph*> : public DefaultDOTGraphTraits {
	DOTGraphTraits (bool isSimple=false) : DefaultDOTGraphTraits(isSimple) {}

	static std::string getGraphName(const IGraph* G) {
	    return "Inequality Graph for '" + G->getName().str();
	}

	static std::string getSimpleNodeLabel(const Node* node,
					      const IGraph *) {
	    if (!node->getName().empty())
		return node->getName().str();

	    std::string Str;
	    raw_string_ostream OS(Str);
	    const Value *value = node->getValue();
#if HAVE_LLVM_VER >= 35
	    OS << value->getName();
#else	    
	    WriteAsOperand(OS, value, false);
#endif	    
	    OS << "FIXME";
	    return OS.str();
	}

	static std::string getCompleteNodeLabel(const Node *node, 
						const IGraph *) {
	    std::string Str;
	    raw_string_ostream OS(Str);
	    if (node->getName().empty()) {
#if HAVE_LLVM_VER >= 35 		    
		OS << node->getValue()->getName();
#else
		WriteAsOperand(OS, node->getValue(), false);
#endif		    
		OS << ", LL(";
#if HAVE_LLVM_VER >= 35 		    
		OS << node->getValue()->getName();
#else
		WriteAsOperand(OS, node->getValue(), false);
#endif		    
		OS << ") = " << node->getLL(node->getValue()) << " : ";
	    }
	    OS << *node->getValue(); 
	    OS << " ";
	    Instruction *insn = dyn_cast<Instruction>(node->getValue());
	    if (insn) {
		for(unsigned int i=0; i < insn->getNumOperands(); i++) {
		    Value *op = insn->getOperand(i);
		    OS << ", LL(";
#if HAVE_LLVM_VER >= 35 		    
		    OS << op->getName();
#else
		    WriteAsOperand(OS, op, false);
#endif		    
		    OS << ") = " << node->getLL(op);
		}
	    }
	    std::string OutStr = OS.str();
	    if (OutStr[0] == '\n') OutStr.erase(OutStr.begin());

	    // Process string output to make it nicer...
	    for (unsigned i = 0; i != OutStr.length(); ++i) {
		if (OutStr[i] == '\n') {                            // Left justify
		    OutStr[i] = '\\';
		    OutStr.insert(OutStr.begin()+i+1, 'l');
		} else if (OutStr[i] == ';') {                      // Delete comments!
		    unsigned Idx = OutStr.find('\n', i+1);            // Find end of line
		    OutStr.erase(OutStr.begin()+i, OutStr.begin()+Idx);
		    --i;
		}
	    }
	    return OutStr;
	}

	std::string getNodeLabel(const Node *node,
				 const IGraph *graph) {
	    if (isSimple())
		return getSimpleNodeLabel(node, graph);
	    else
		return getCompleteNodeLabel(node, graph);
	}

	static std::string getEdgeSourceLabel(const Node *node,
					      vector<Node*>::const_iterator I) {
#if 0
	    // Label source of conditional branches with "T" or "F"
	    if (const BranchInst *BI = dyn_cast<BranchInst>(Node->getTerminator()))
		if (BI->isConditional())
		    return (I == succ_begin(Node)) ? "T" : "F";
    
	    // Label source of switch edges with the associated value.
	    if (const SwitchInst *SI = dyn_cast<SwitchInst>(Node->getTerminator())) {
		unsigned SuccNo = I.getSuccessorIndex();

		if (SuccNo == 0) return "def";
      
		std::string Str;
		raw_string_ostream OS(Str);
		SwitchInst::ConstCaseIt Case =
		    SwitchInst::ConstCaseIt::fromSuccessorIndex(SI, SuccNo); 
		OS << Case.getCaseValue()->getValue();
		return OS.str();
	    }    
#endif
	    return "";
	}
    };
}

#endif // _IGRAPH_H_

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
#include "llvmGlobalToWide.h"
#include "llvm/Support/GenericDomTree.h"
#include "llvm/Support/GenericDomTreeConstruction.h"

#if HAVE_LLVM_VER >= 35
#else
#include "llvm/Assembly/Writer.h"
#endif

#include <vector>
#include <string>
using namespace std;
using namespace llvm;


enum NodeKind {
    NODE_DEF,
    NODE_USE,
    NODE_PHI,
    NODE_NONE
};

class Node {
private:
    // 
    NodeKind kind;    
    Value* value;
    Instruction* insn;
    unsigned int version;
    int locality;
    //
    vector<Node*> children;
    vector<Node*> parents;

public:
    Node(NodeKind _kind, Value* _value, Instruction* _insn, unsigned int _version, int _locality) {
	kind = _kind;	
	value = _value;
	insn = _insn;
	version = _version;
	locality = _locality;
	
    };

    // parents
    vector<Node*>::iterator parents_begin() { return parents.begin(); }
    vector<Node*>::iterator parents_end() { return parents.end(); }
    vector<Node*>::const_iterator parents_begin() const { return parents.begin(); }
    vector<Node*>::const_iterator parents_end() const { return parents.end(); }
    // children
    vector<Node*>::iterator begin() { return children.begin(); }
    vector<Node*>::iterator end() { return children.end(); }
    vector<Node*>::const_iterator begin() const { return children.begin(); }
    vector<Node*>::const_iterator end() const { return children.end(); }
    
    void addParents(Node* parent) {
	vector<Node*>::iterator I = find(parents.begin(), parents.end(), parent);
        if( I == parents.end() ){	    
	    parents.push_back(parent);
	    parent->addChild(this);
	}
    }

    void addChild(Node *child) { 
	vector<Node*>::iterator I = find(children.begin(), children.end(), child);
        if( I == children.end() ){
	    children.push_back(child); 
	    child->addParents(this);
	}
    }

    Value* getValue() const { return value; }
    NodeKind getKind() const { return kind; }
    unsigned int getVersion() const { return version; }
    int getLocality() const { return locality; }

    void dump() {
	errs () << this->getLocality() << " : ";
	this->getValue()->dump();	
    }

    void printAsOperand(raw_ostream &o, bool) {

    }
    
};

class IGraph {
private:
    StringRef name;
    // required?
    Node* entry;    
    vector<Node*> nodes;
    // required?
    vector<Node*> getRootNodes() { return nodes; }
    Value* getOperandIfLocalStmt(Instruction *insn);

    //
    bool debug = true;
    
public:
    IGraph (StringRef _name) { name = _name; }
    void construct(Function *F, GlobalToWideInfo *info);
    
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
    // for Graph Traits
    // Graph Traits requires you to provide the following
    // (for more details see "llvm/ADT/GraphTraits.h") :

    // typedef NodeType          - Type of Node in the graph
    // typedef ChildIteratorType - Type used to iterate over children in graph    
    // static NodeType *getEntryNode(const GraphType &)
    //    Return the entry node of the graph    
    // static ChildIteratorType child_begin(NodeType *)
    // static ChildIteratorType child_end  (NodeType *)
    //    Return iterators that point to the beginning and ending of the child
    //    node list for the specified node.
    //        
    // typedef  ...iterator nodes_iterator;
    // static nodes_iterator nodes_begin(GraphType *G)
    // static nodes_iterator nodes_end  (GraphType *G)
    //    nodes_iterator/begin/end - Allow iteration over all nodes in the graph    
    // static unsigned       size       (GraphType *G)
    //    Return total number of nodes in the graph

    
    // template specialization for <Node*>
    template<> struct GraphTraits<Node*> {
	typedef Node NodeType;
	typedef std::vector<Node*>::iterator ChildIteratorType;
		
	static NodeType *getEntryNode(Node *node) { return node; }
	static inline ChildIteratorType child_begin(NodeType *N) { return N->begin(); }
	static inline ChildIteratorType child_end(NodeType *N) { return N->end(); }
    };

    // template specialization for <const Node*>
    template<> struct GraphTraits<const Node*> {
	typedef const Node NodeType;
	typedef vector<Node*>::const_iterator ChildIteratorType;
	
	static NodeType *getEntryNode(const Node *node) { return node; }
	static inline ChildIteratorType child_begin(const NodeType *N) { return N->begin(); }
	static inline ChildIteratorType child_end(const NodeType *N) { return N->end(); }
    };

    // template specialization for <IGraph*>
    template<> struct GraphTraits<IGraph*> : public GraphTraits<Node*> {
	static NodeType *getEntryNode(IGraph *G) { return G->getEntry(); }
	typedef std::vector<Node*>::iterator nodes_iterator;

	static nodes_iterator nodes_begin(IGraph *G) { return G->begin(); }
	static nodes_iterator nodes_end(IGraph *G) { return G->end(); }
	static unsigned size(IGraph *G) { return G->size(); };
    };

    // template specialization for <const IGraph*>
    template<> struct GraphTraits<const IGraph*> : public GraphTraits<const Node*> {
	static NodeType *getEntryNode(const IGraph *G) { return G->getEntry(); }
	typedef vector<Node*>::const_iterator nodes_iterator;

	static nodes_iterator nodes_begin(const IGraph *G) { return G->begin(); }
	static nodes_iterator nodes_end(const IGraph *G) { return G->end(); }
	static unsigned size(const IGraph *G) { return G->size(); }
    };
    

    // template specialization for <const IGraph*> for Write Graph
    template<> struct DOTGraphTraits<const IGraph*> : public DefaultDOTGraphTraits {
	DOTGraphTraits (bool isSimple=false) : DefaultDOTGraphTraits(isSimple) {}

	static std::string getGraphName(const IGraph* G) {
	    return "Inequality Graph for '" + G->getName().str();
	}

	static std::string getSimpleNodeLabel(const Node* node,
					      const IGraph *) {
	    if (!node->getValue()->getName().empty())
		return node->getValue()->getName().str();

	    std::string Str;
	    raw_string_ostream OS(Str);
	    const Value *value = node->getValue();
#if HAVE_LLVM_VER >= 35
	    value->printAsOperand(OS, false);
#else	    
	    WriteAsOperand(OS, value, false);
#endif	    

	    return OS.str();
	}

	static std::string getCompleteNodeLabel(const Node *node, 
						const IGraph *) {
	    std::string Str;
	    raw_string_ostream OS(Str);
	    Value* value = node->getValue();
	    	    
	    // print Node Information
	    switch (node->getKind()) {
	    case NODE_DEF:
#if HAVE_LLVM_VER >= 35
	    value->printAsOperand(OS, false);
#else	    
	    WriteAsOperand(OS, value, false);
#endif
	    OS << "_" << node->getVersion() << " = " << node->getLocality();
	    break;
	    case NODE_USE:
		OS << "... = ";
#if HAVE_LLVM_VER >= 35
	    value->printAsOperand(OS, false);
#else	    
	    WriteAsOperand(OS, value, false);
#endif	    
	    OS << "_" << node->getVersion();
		break;
	    case NODE_PHI:
		break;
	    default:
		assert(0 && "Inequality Graph Node Type should not be NODE_NONE");
	    }

	    std::string OutStr = OS.str();
	    // 
	    if (OutStr[0] == '\n') OutStr.erase(OutStr.begin());

	    // Process OutStr for DOT format
	    for (unsigned i = 0; i != OutStr.length(); ++i) {
		if (OutStr[i] == '\n') {        
		    OutStr[i] = '\\';
		    OutStr.insert(OutStr.begin()+i+1, 'l');
		} else if (OutStr[i] == ';') {
                    // Delete comments!
		    unsigned Idx = OutStr.find('\n', i+1);
                    // Find end of line
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
	    return "";
	}
    };       
}


#endif // _IGRAPH_H_

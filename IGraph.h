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
#include "llvm/ADT/SmallBitVector.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/DOTGraphTraits.h"
#include "llvm/Support/raw_ostream.h"
#include "llvmUtil.h"
#include "llvmGlobalToWide.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/GenericDomTree.h"
#include "llvm/Support/GraphWriter.h"

#if HAVE_LLVM_VER >= 35
#else
#include "llvm/Assembly/Writer.h"
#endif

#include <vector>
#include <string>
using namespace std;
using namespace llvm;

enum NodeKind {
    NODE_ENTRY,
    NODE_DEF,
    NODE_USE,
    NODE_PHI,
    NODE_PI,
    NODE_NONE
};


class Node {
public:
    typedef BitVector IDominatorTreeType;
    typedef SmallVector<Node*, 32> DominanceFrontierType;   
private:
    // 
    NodeKind kind;    
    Value* value;
    Instruction* insn;
    unsigned int version;
    int locality;
    // for Dominant Tree
    int postOrderNumber;

    //
    typedef std::vector<Node*> NodeElementType;

    // 
    NodeElementType children;
    NodeElementType parents;

    // For Dominator Tree & Dominance Frontier
    IDominatorTreeType idom;
    bool domIsUndefined;
    DominanceFrontierType dominanceFrontier;
    
public:
    
    Node(NodeKind _kind,
	 Value* _value,
	 Instruction* _insn,
	 unsigned int _version,
	 int _locality) {
	kind = _kind;	
	value = _value;
	insn = _insn;
	version = _version;
	locality = _locality;
	postOrderNumber = -1;
	domIsUndefined = true;
    };

    typedef NodeElementType::iterator iterator;
    typedef NodeElementType::const_iterator const_iterator;
    
    // parents
    iterator parents_begin() { return parents.begin(); }
    iterator parents_end() { return parents.end(); }
    const_iterator parents_begin() const { return parents.begin(); }
    const_iterator parents_end() const { return parents.end(); }

    // children
    iterator children_begin() { return children.begin(); }
    iterator children_end() { return children.end(); }
    const_iterator children_begin() const { return children.begin(); }
    const_iterator children_end() const { return children.end(); }
    
    void addParents(Node* parent) {
	iterator I = find(parents_begin(), parents_end(), parent);
        if( I == parents_end() ){
	    parents.push_back(parent);
	    parent->addChild(this);
	}
    }

    void addChild(Node *child) { 
	iterator I = find(children_begin(), children_end(), child);
        if( I == children.end() ){
	    children.push_back(child);
	    child->addParents(this);
	}
    }

    Value* getValue() const { return value; }
    NodeKind getKind() const { return kind; }
    unsigned int getVersion() const { return version; }   
    int getLocality() const { return locality; }

    // For Dominator Tree & Dominance Frontier
    void setPostOrderNumber(int _postOrderNumber) { postOrderNumber = _postOrderNumber; }
    int getPostOrderNumber() const { return postOrderNumber; }
    bool getUndefined() { return domIsUndefined; }
    void setUndefined(bool flag) { domIsUndefined = flag; } 
    IDominatorTreeType getDom() { return idom; }
    void setDom(IDominatorTreeType _idom) { idom = _idom; }
    void resetDominanceFrontier() { dominanceFrontier.clear(); }
    void addToDominanceFrontier(Node *b) { dominanceFrontier.push_back(b); }
	
    int getNumPreds() { return parents.size(); }

    typedef DominanceFrontierType::iterator df_iterator;

    df_iterator df_begin() { return dominanceFrontier.begin(); }
    df_iterator df_end() { return dominanceFrontier.end(); }
	
    // for debug
    void dump() const {
	printAsOperand(errs(), true);
	errs () << "\n";
    }

    void printAsOperand(raw_ostream &o, bool) const {
	// print Node Information
	switch (this->getKind()) {
	case NODE_ENTRY:
	    o << "entry";
	    break;
	case NODE_DEF:
#if HAVE_LLVM_VER >= 35
	    value->printAsOperand(o, false);
#else	    
	    WriteAsOperand(o, value, false);
#endif
	    o << "_" << this->getVersion() << " = " << this->getLocality();
	    break;
	case NODE_USE:
	    o << "... = ";
#if HAVE_LLVM_VER >= 35
	    value->printAsOperand(o, false);
#else	    
	    WriteAsOperand(o, value, false);
#endif	    
	    o << "_" << this->getVersion();
	    break;
	case NODE_PHI:
	case NODE_PI:
	    break;
	default:
		assert(0 && "Inequality Graph Node Type should not be NODE_NONE");
	}

	o << "\n" << this->getPostOrderNumber();
    }
    
};

class IGraph {
private:
    // the name of IGraph, which is used for DOT graph generation
    StringRef name;
    // entry node
    Node* entry;
    // List of nodes in IGraph
    typedef std::vector<Node*> NodeListType;
    NodeListType nodes;
    
    NodeListType getRootNodes() { return nodes; }
    Value* getOperandIfLocalStmt(Instruction *insn);

    void setPostOrderNumberWithDFSImpl(Node*, int&);
    void setPostOrderNumberWithDFS();

    /* For Dominator Tree Construction*/
    void computeDominatorTree();
    Node* computeIntersect(Node*, Node*);

    /* For Dominance Frontier Construction */
    void computeDominanceFrontier();
    

    //
    bool debug = true;
    
public:
    IGraph (StringRef _name) { name = _name; }
    void construct(Function *F, GlobalToWideInfo *info);
    
    Node* getEntry() const { return entry; }
    StringRef getName() const { return name; }

    typedef NodeListType::iterator iterator;
    typedef NodeListType::const_iterator const_iterator;

    // Iterator for enumerating nodes of IGraph.
    iterator begin() { return nodes.begin(); }
    const_iterator begin() const { return nodes.begin(); }
    iterator end() { return nodes.end(); }
    const_iterator end() const { return nodes.end(); }

    Node* getNodeByValue(const Value* v) { 
	for (NodeListType::iterator I = nodes.begin(), E = nodes.end(); I != E; I++) {
	    Node* tmp = *I;
	    if (v == tmp->getValue()) {
		return tmp;
	    }
	}
	return NULL;
    }

    Node* getNodeByPostOrderNumber(const int number) { 
	for (NodeListType::iterator I = nodes.begin(), E = nodes.end(); I != E; I++) {
	    Node* tmp = *I;
	    if (number == tmp->getPostOrderNumber()) {
		return tmp;
	    }
	}
	return NULL;
    }
    
    void addNode(Node* n) { nodes.push_back(n); }
    unsigned size() const { return nodes.size(); }

    // for Debug
    void dumpDOT();
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
    /* The followings are used for DOT Graph generation by DOTGraphTraits */

    // template specialization for <const Node*>
    template<> struct GraphTraits<const Node*> {
	typedef const Node NodeType;
	typedef NodeType::const_iterator ChildIteratorType;
	
	static NodeType *getEntryNode(const Node *node) { return node; }
	static inline ChildIteratorType child_begin(const NodeType *N) { return N->children_begin(); }
	static inline ChildIteratorType child_end(const NodeType *N) { return N->children_end(); }
    };
    
    // template specialization for <const IGraph*>
    template<> struct GraphTraits<const IGraph*> : public GraphTraits<const Node*> {
	static NodeType *getEntryNode(const IGraph *G) { return G->getEntry(); }
	typedef IGraph::const_iterator nodes_iterator;

	static nodes_iterator nodes_begin(const IGraph *G) { return G->begin(); }
	static nodes_iterator nodes_end(const IGraph *G) { return G->end(); }
	static unsigned size(const IGraph *G) { return G->size(); }
    };
    
    // template specialization for <const IGraph*> for Writing DOTGraph
    template<> struct DOTGraphTraits<const IGraph*> : public DefaultDOTGraphTraits {
	DOTGraphTraits (bool isSimple=false) : DefaultDOTGraphTraits(isSimple) {}

	static std::string getGraphName(const IGraph* G) {
	    return "Inequality Graph for '" + G->getName().str();
	}

	std::string getNodeLabel(const Node *node,
				 const IGraph *graph) {
	    std::string Str;
	    raw_string_ostream OS(Str);
	    Value* value = node->getValue();
	    
	    node->printAsOperand(OS, true);
	    std::string OutStr = OS.str();
	    
	    // Erase  
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

	static std::string getEdgeSourceLabel(const Node *node,
					      Node::const_iterator I) {
	    return "";
	}
    };       
}


#endif // _IGRAPH_H_

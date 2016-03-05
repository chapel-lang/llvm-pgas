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

class Node {
public:
    typedef BitVector IDominatorTreeType;
    typedef SmallVector<Node*, 32> DominanceFrontierType;
    typedef std::vector<Node*> NodeElementType;

    enum NodeKind {
	NODE_ENTRY,
	NODE_DEF,
	NODE_USE,
	NODE_PHI,
	NODE_PI,
	NODE_NONE
    };
   
private:
    // Kind of this node (e.g. DEF/USE/PHI)
    NodeKind kind;    
    // Correspoing possibly-remote pointer
    Value* value;
    // Corresponding instruction
    Instruction* insn;
    // used in Locality-SSA (live-range splitting)
    unsigned int version;
    // Current convention (0: definitely-local, 100: possibly-remote)
    int locality;
    // For visiting nodes in post order
    int postOrderNumber;
    // Immediate dominator of this node
    IDominatorTreeType idom;
    // Used for dominator tree calculation
    bool domIsUndefined;
    // Dominance frontiers of this node
    DominanceFrontierType dominanceFrontier;
    
    // Children and Parents of this node
    NodeElementType children;
    NodeElementType parents;

    // Used for showing informtion on this node
    void printAsOperandInternal(raw_ostream &o, Value* value) const;

public:
    // Constructor
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

    // For enumerating parents/children of this node
    typedef NodeElementType::iterator iterator;
    typedef NodeElementType::const_iterator const_iterator;
    
    // Interface for enumerating parents
    iterator parents_begin() { return parents.begin(); }
    iterator parents_end() { return parents.end(); }
    const_iterator parents_begin() const { return parents.begin(); }
    const_iterator parents_end() const { return parents.end(); }

    // Interface for enumerating children
    iterator children_begin() { return children.begin(); }
    iterator children_end() { return children.end(); }
    const_iterator children_begin() const { return children.begin(); }
    const_iterator children_end() const { return children.end(); }

    // Getter for general node information
    Value* getValue() const { return value; }
    Instruction* getInstruction() const { return insn; }
    NodeKind getKind() const { return kind; }
    unsigned int getVersion() const { return version; }
    int getLocality() const { return locality; }
    int getNumPreds() { return parents.size(); }

    // Setter for node information
    void setVersion(unsigned int _version) { version = _version; }   

    void addParent(Node* parent) {
	iterator I = find(parents_begin(), parents_end(), parent);
        if( I == parents_end() ){
	    parents.push_back(parent);
	}
    }

    void eraseFromParent(Node* parent) {
	iterator I = find(parents_begin(), parents_end(), parent);
	if ( I != parents_end() ) {
	    parents.erase(I);
	}
    }

    void addChild(Node *child) { 
	iterator I = find(children_begin(), children_end(), child);
        if( I == children.end() ){
	    children.push_back(child);
	}
    }

    void eraseFromChild(Node* child) {
	iterator I = find(children_begin(), children_end(), child);
	if ( I != children_end() ) {
	    children.erase(I);
	}
    }

    /* === Utility functions for Inequality Graph Construction Starts === */

    // For enumerating Dominance frontiers of this node
    typedef DominanceFrontierType::iterator df_iterator;

    // Interface for enumerating Dominance frontiers of this node
    df_iterator df_begin() { return dominanceFrontier.begin(); }
    df_iterator df_end() { return dominanceFrontier.end(); }    

    // Used for visiting nodes in post order
    void resetPostOrderNumber() { postOrderNumber = -1; };
    void setPostOrderNumber(int _postOrderNumber) {
	postOrderNumber = _postOrderNumber; }
    int getPostOrderNumber() const { return postOrderNumber; }
    // Used for calculating dominator tree
    bool getUndefined() { return domIsUndefined; }
    void setUndefined(bool flag) { domIsUndefined = flag; } 
    // Setter/Getter for immediate Dominator
    IDominatorTreeType getIDom() { return idom; }
    void setIDom(IDominatorTreeType _idom) { idom = _idom; }
    // Dominance Frontier
    void resetDominanceFrontier() { dominanceFrontier.clear(); }
    void addToDominanceFrontier(Node *b) { dominanceFrontier.push_back(b); }
	
    /* === Utility functions for Inequality Graph Construction Ends === */

    // Used for showing information on this node
    // (e.g. when dumping in DOT format) 
    void printAsOperand(raw_ostream&, bool) const;
    
    // Used for debug
    void dump() const {
	printAsOperand(errs(), true);
	errs () << "\n";
    }    
};

class IGraph {
private:
    // The name of IGraph, which is used for DOT graph generation
    StringRef name;

    // Entry node
    Node* entry;

    // Nodes of IGraph
    typedef std::vector<Node*> NodeListType;
    NodeListType nodes;
    
    /* === Data Structures for Inequality Graph Construction Starts === */

    // An array of possibly remote pointers
    typedef SmallVector<Value*, 128> PossiblyRemoteArrayType;
    PossiblyRemoteArrayType possiblyRemotePtrs;
    PossiblyRemoteArrayType possiblyRemoteArgs;

    // Used for analyzing def/use of locality
    typedef DenseMap<Instruction*, std::tuple<Node::NodeKind, Value*, 
Instruction*, int>> InsnToNodeMapType;

    // For Renaming
    typedef std::stack<int> StackType;
    typedef DenseMap<Value*, StackType> RenamingStacksType;
    typedef DenseMap<Value*, int> RenamingCounterType;
    RenamingStacksType renamingStacks;
    RenamingCounterType renamingCounters;

    /* === Data Structures for Inequality Graph Construction Ends === */

    /* === Utility functions for Inequality graph construction Starts === */

    // Add a node to the graph
    void addNode(Node* n) { nodes.push_back(n); }

    // Language specific 
    std::pair<bool, Value*> isChapelLocalStmt(Instruction *insn);

    // For Initial IGraph construction from LLVM Function
    InsnToNodeMapType analyzeDefUseOfLocality(Function *, GlobalToWideInfo *);
    void buildGraph(Function*, InsnToNodeMapType&);

    // For constructing Locality-SSA in IGraph
    void calculateDTandDF();
    void setPostOrderNumberWithDFSInternal(Node*, int&, Node::NodeElementType&);
    void setPostOrderNumberWithDFS();
    void computeDominatorTree();
    Node* computeIntersect(Node*, Node*);
    void computeDominanceFrontier();   
    void performPhiNodeInsertion(bool&);
    void performRenaming();
    void performRenamingInternal(Node *, Node::NodeElementType&);
    void generateName(Value *v);

    /* === Utility functions for Inequality graph construction Ends === */

    // verbose
    bool debug = true;
    
public:
    // Constructor 
    IGraph () { name = "IGraph"; }
    // Constructor with name
    IGraph (StringRef _name) { name = _name; }

    // For enumerating nodes in the graph
    typedef NodeListType::iterator iterator;
    typedef NodeListType::const_iterator const_iterator;

    // Interface for enumerating nodes of IGraph.
    iterator begin() { return nodes.begin(); }
    const_iterator begin() const { return nodes.begin(); }
    iterator end() { return nodes.end(); }
    const_iterator end() const { return nodes.end(); }

    // Getter
    Node* getEntry() const { return entry; }
    StringRef getName() const { return name; }
    unsigned size() const { return nodes.size(); }

    Node* getNodeByPostOrderNumber(const int number) { 
	for (NodeListType::iterator I = nodes.begin(),
		 E = nodes.end(); I != E; I++) {
	    Node* tmp = *I;
	    if (number == tmp->getPostOrderNumber()) {
		return tmp;
	    }
	}
	return NULL;
    }

    // Construct a inequality graph from an LLVM function
    void construct(Function *F, GlobalToWideInfo *info);

    //
    Node* getDefNode(const Node*) const;
    
    enum Answer {
	TRUE,
	FALSE,
	UNKNOWN	
    };

    //   
    Answer prove(const Value*, const Instruction *, const int) const;
    Answer proveUpward(const Node*, const int) const;
    
    // Used for dumping IGraph in DOT format
    void dumpDOT();
};

/* The followings are used for DOT Graph generation by llvm::DOTGraphTraits */
namespace llvm {
    // for Graph Traits
    // (for more details see "llvm/ADT/GraphTraits.h")

    // template specialization for <const Node*>
    template<> struct GraphTraits<const Node*> {
	typedef const Node NodeType;
	typedef NodeType::const_iterator ChildIteratorType;
	
	static NodeType *getEntryNode(const Node *node) { return node; }
	static inline ChildIteratorType child_begin(const NodeType *N) {
	    return N->children_begin();
	}
	static inline ChildIteratorType child_end(const NodeType *N) {
	    return N->children_end();
	}
    };
    
    // template specialization for <const IGraph*>
    template<> struct GraphTraits<const IGraph*> :
	public GraphTraits<const Node*> {
	static NodeType *getEntryNode(const IGraph *G) {
	    return G->getEntry();
	}
	typedef IGraph::const_iterator nodes_iterator;

	static nodes_iterator nodes_begin(const IGraph *G) {
	    return G->begin();
	}
	static nodes_iterator nodes_end(const IGraph *G) {
	    return G->end();
	}
	static unsigned size(const IGraph *G) { return G->size(); }
    };
    
    // template specialization for <const IGraph*> for Writing DOTGraph
    template<> struct DOTGraphTraits<const IGraph*> : public 
	DefaultDOTGraphTraits {
	DOTGraphTraits (bool isSimple=false) : DefaultDOTGraphTraits(isSimple) 
	{}

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

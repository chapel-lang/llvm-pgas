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

using namespace std;
using namespace llvm;

unsigned int
Node::getAddressSpace(Value* v)
{
    PointerType* pt = dyn_cast<PointerType>(v->getType());
    if(pt) {
	return pt->getAddressSpace();
    } else {
	return 0;
    }
}

void
Node::initLLMap()
{
    LLMap[value] = getAddressSpace(value);
    // Instruction
    Instruction *insn = dyn_cast<Instruction>(value);
    if (!insn) return;
    if (insn->getOpcode() != Instruction::Call) {
	for(unsigned int i=0; i < insn->getNumOperands(); i++) {
	    Value *op = insn->getOperand(i);
	    LLMap[op] = getAddressSpace(op);
	}
    }
    switch(insn->getOpcode()) {
    case Instruction::Call: {
	CallInst *call = cast<CallInst>(insn);
	Function* f = call->getCalledFunction();
	if (f != NULL) {
	    /* *
	     * We are assuming that gf.addr function calls correspond to Chapel's local statements, but this is not always true because gf.addr is also used to extract a local pointer from a wide pointer. We work on this later pass (see exemptionTest in llvmLocalityOptimization.cpp). 
	     */  
	    if (f->getName().startswith(".gf.addr")) {
		// Argument of ".gf.addr" is definitely local
		for (unsigned int i = 0; i < call->getNumArgOperands(); i++) {
		    Value* v = call->getArgOperand(i);
		    LLMap[v] = 0;
		}
 	    }
	}
	break;
    }
    }    
}

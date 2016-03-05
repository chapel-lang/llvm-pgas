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
// This is a reduced version of the original LLVM global value numbering pass. (GVN.cpp)
// This pass is only used for assigning a value number to variables and expressions and 
// does not perform any CSE (common subexpression elimination)
//===----------------------------------------------------------------------===//

#ifndef _VALUE_TABLE_H_
#define _VALUE_TABLE_H_

#include "llvm/IR/Value.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Hashing.h"

#include "llvmUtil.h"

using namespace llvm;

struct Expression {
    uint32_t opcode;
    Type *type;
    SmallVector<uint32_t, 4> varargs;
    
    Expression(uint32_t o = ~2U) : opcode(o) { }
    
    bool operator==(const Expression &other) const {
	if (opcode != other.opcode)
	    return false;
	if (opcode == ~0U || opcode == ~1U)
	    return true;
	if (type != other.type)
	    return false;
	if (varargs != other.varargs)
	    return false;
	return true;
    }

    // ignore type equality
    bool equals(const Expression &other) const {
	if (opcode != other.opcode)
	    return false;
	if (opcode == ~0U || opcode == ~1U)
	    return true;
	if (varargs != other.varargs)
	    return false;
	return true;
    }
    
    friend hash_code hash_value(const Expression &Value) {
	return hash_combine(Value.opcode, Value.type,
			    hash_combine_range(Value.varargs.begin(),
					       Value.varargs.end()));
    }
    
    void dump() {
	errs () << "Ope" << opcode << " ";
	for (SmallVector<uint32_t, 4>::iterator I = varargs.begin(), E = varargs.end(); I != E; I++) {
	    uint32_t val = *I;
	    errs () << val << ", ";
	}
	errs () << "\n";
    }
};

namespace llvm {
    template <> struct DenseMapInfo<Expression> {
	static inline Expression getEmptyKey() {
	    return ~0U;
	}

	static inline Expression getTombstoneKey() {
	    return ~1U;
	}

	static unsigned getHashValue(const Expression e) {
	    using llvm::hash_value;
	    return static_cast<unsigned>(hash_value(e));
	}
	static bool isEqual(const Expression &LHS, const Expression &RHS) {
	    return LHS == RHS;
	}
    };

}

class ValueTable {
    DenseMap<Value*, uint32_t> valueNumbering;
    DenseMap<Expression, uint32_t> expressionNumbering;
    uint32_t nextValueNumber;
    
    Expression create_expression(Instruction* I);
    uint32_t lookup_or_add_call(CallInst* C);

public:
    ValueTable() : nextValueNumber(1) { }

    uint32_t lookup_or_add(Value *V);
    uint32_t lookup(Value *V) const;

    Value* lookup_value(uint32_t id) { 
	for (DenseMap<Value*, uint32_t>::const_iterator
		 I = valueNumbering.begin(), E = valueNumbering.end(); I != E; ++I) {
	    Value* val = I->first;
	    uint32_t num = I->second;
	    if (num == id) {
		return val;
	    }		
	}
	return NULL;
    }

    bool sameExpressions(Instruction *I1, Instruction *I2) {
	Expression e1 = create_expression(I1);
	Expression e2 = create_expression(I2);
	if (e1.equals(e2)) {
	    return true;
	} else {
	    return false;
	}
    }
    
    void add(Value *V, uint32_t num);
    void clear();
    void erase(Value *v);

    uint32_t getNextUnusedValueNumber() { return nextValueNumber; }
    void verifyRemoved(const Value *) const;

    void dump() {
	errs () << "[Value Table Dump Starts]\n";
	for (DenseMap<Value*, uint32_t>::const_iterator
		 I = valueNumbering.begin(), E = valueNumbering.end(); I != E; ++I) {
	    Value* val = I->first;
	    uint32_t num = I->second;
	    errs () << num << " : " << *val << "\n";		
	}
	errs () << "[Value Table Dump Ends]\n";
	errs () << "[Expresion Table Dump Starts]\n";
	for (DenseMap<Expression, uint32_t>::const_iterator
		 I = expressionNumbering.begin(), E = expressionNumbering.end(); I != E; ++I) {
	    Expression val = I->first;
	    uint32_t num = I->second;
	    //errs () << num << " : Ope" << val.opcode << "\n";
	    errs () << num << " : ";
	    val.dump();
	}
	errs () << "[Expresion Table Dump Ends]\n";	   
    }
};
#endif

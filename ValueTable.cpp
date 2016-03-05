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

#include "ValueTable.h"
#include "llvmGlobalToWide.h"

Expression ValueTable::create_expression(Instruction *I) {
    Expression e;
    e.type = I->getType();
    e.opcode = I->getOpcode();
    for (Instruction::op_iterator OI = I->op_begin(), OE = I->op_end();
	 OI != OE; ++OI)
	e.varargs.push_back(lookup_or_add(*OI));
    if (I->isCommutative()) {
	// Ensure that commutative instructions that only differ by a permutation
	// of their operands get the same value number by sorting the operand value
	// numbers.  Since all commutative instructions have two operands it is more
	// efficient to sort by hand rather than using, say, std::sort.
	assert(I->getNumOperands() == 2 && "Unsupported commutative instruction!");
	if (e.varargs[0] > e.varargs[1])
	    std::swap(e.varargs[0], e.varargs[1]);
    }

    if (CmpInst *C = dyn_cast<CmpInst>(I)) {
	// Sort the operand value numbers so x<y and y>x get the same value number.
	CmpInst::Predicate Predicate = C->getPredicate();
	if (e.varargs[0] > e.varargs[1]) {
	    std::swap(e.varargs[0], e.varargs[1]);
	    Predicate = CmpInst::getSwappedPredicate(Predicate);
	}
	e.opcode = (C->getOpcode() << 8) | Predicate;
    } else if (InsertValueInst *E = dyn_cast<InsertValueInst>(I)) {
	for (InsertValueInst::idx_iterator II = E->idx_begin(), IE = E->idx_end();
	     II != IE; ++II)
	    e.varargs.push_back(*II);
    }

    return e;
}

//===----------------------------------------------------------------------===//
//                     ValueTable External Functions
//===----------------------------------------------------------------------===//

/// add - Insert a value into the table with a specified value number.
void ValueTable::add(Value *V, uint32_t num) {
    valueNumbering.insert(std::make_pair(V, num));
}

uint32_t ValueTable::lookup_or_add_call(CallInst *C) {
    Function *F = C->getCalledFunction();
    if (F != NULL && F->hasName() && F->getName().startswith(GLOBAL_FN_GLOBAL_ADDR)) {
	Value *op = C->getOperand(0);
	DenseMap<Value*, uint32_t>::const_iterator VI = valueNumbering.find(op);
	if (VI != valueNumbering.end()) {
	    return VI->second;
	} else {
	    // Not numbered yet
	    return lookup_or_add(op);
	}
    } else {
	valueNumbering[C] = nextValueNumber;
	return nextValueNumber++;
    }
}

/// lookup_or_add - Returns the value number for the specified value, assigning
/// it a new number if it did not have one before.
uint32_t ValueTable::lookup_or_add(Value *V) {
    DenseMap<Value*, uint32_t>::iterator VI = valueNumbering.find(V);
    if (VI != valueNumbering.end())
	return VI->second;

    if (!isa<Instruction>(V)) {
//	errs () << *V << " => " << nextValueNumber << "\n";
	valueNumbering[V] = nextValueNumber;
	return nextValueNumber++;
    }
   
    Instruction* I = cast<Instruction>(V);
    Expression exp;
    switch (I->getOpcode()) {
    case Instruction::Call:
	return lookup_or_add_call(cast<CallInst>(I));
    case Instruction::Add:
    case Instruction::FAdd:
    case Instruction::Sub:
    case Instruction::FSub:
    case Instruction::Mul:
    case Instruction::FMul:
    case Instruction::UDiv:
    case Instruction::SDiv:
    case Instruction::FDiv:
    case Instruction::URem:
    case Instruction::SRem:
    case Instruction::FRem:
    case Instruction::Shl:
    case Instruction::LShr:
    case Instruction::AShr:
    case Instruction::And:
    case Instruction::Or:
    case Instruction::Xor:
    case Instruction::ICmp:
    case Instruction::FCmp:
    case Instruction::Trunc:
    case Instruction::ZExt:
    case Instruction::SExt:
    case Instruction::FPToUI:
    case Instruction::FPToSI:
    case Instruction::UIToFP:
    case Instruction::SIToFP:
    case Instruction::FPTrunc:
    case Instruction::FPExt:
    case Instruction::PtrToInt:
    case Instruction::IntToPtr:
    case Instruction::BitCast:
    case Instruction::Select:
    case Instruction::ExtractElement:
    case Instruction::InsertElement:
    case Instruction::ShuffleVector:
    case Instruction::InsertValue:
    case Instruction::GetElementPtr:
	exp = create_expression(I);
	break;
    case Instruction::ExtractValue:
//	exp = create_extractvalue_expression(cast<ExtractValueInst>(I));
//	break;
    default:
	valueNumbering[V] = nextValueNumber;
	return nextValueNumber++;
    }

    uint32_t& e = expressionNumbering[exp];
    if (!e) e = nextValueNumber++;
    valueNumbering[V] = e;
    return e;
}

/// lookup - Returns the value number of the specified value. Fails if
/// the value has not yet been numbered.
uint32_t ValueTable::lookup(Value *V) const {
    DenseMap<Value*, uint32_t>::const_iterator VI = valueNumbering.find(V);
    assert(VI != valueNumbering.end() && "Value not numbered?");
    return VI->second;
}

/// clear - Remove all entries from the ValueTable.
void ValueTable::clear() {
    valueNumbering.clear();
    expressionNumbering.clear();
    nextValueNumber = 1;
}

/// erase - Remove a value from the value numbering.
void ValueTable::erase(Value *V) {
    valueNumbering.erase(V);
}

/// verifyRemoved - Verify that the value is removed from all internal data
/// structures.
void ValueTable::verifyRemoved(const Value *V) const {
    for (DenseMap<Value*, uint32_t>::const_iterator
	     I = valueNumbering.begin(), E = valueNumbering.end(); I != E; ++I) {
	assert(I->first != V && "Inst still occurs in value numbering map!");
    }
}

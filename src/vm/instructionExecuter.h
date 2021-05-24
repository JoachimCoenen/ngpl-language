#ifndef INSTRUCTIONEXECUTER_H
#define INSTRUCTIONEXECUTER_H

#include "callStack.h"
#include "instruction.h"
#include "value.h"

#include "cat_utils.h"
#include "cat_stack.h"
#include "cat_exception.h"

#include "tsl/ordered_set.h"

#include <unordered_map>
#include <vector>

namespace ngpl {

class ExecutionError : public cat::Exception
{
	// TODO: maybe add token argument to SyntaxError?
public:
	ExecutionError(const cat::String& message, const Instruction& instruction);

	const Instruction& instruction() const { return _instruction; }
	const cat::String& rawMessage() const { return _rawMessage; }

	virtual ExecutionError* makeCopy() const override {
		return new ExecutionError(*this);
	}

protected:
	Instruction _instruction;
	cat::String _rawMessage;
};


class InstructionExecuter
{
public:
	InstructionExecuter(const std::vector<Instruction>& instructions, uint64_t programmCounter = 0);

	void executeInstruction();
	void run();

//protected:
	std::vector<Instruction> _instructions;
	uint64_t _programmCounter = 0;
	uint64_t _overallCounter = 0;
	uint64_t _funcCallCounter = 0;

	// std::vector<Value> _values;
	//cat::Stack<Value> _temporaryStack;
	CallStack _stack;
	cat::DynArray<Value>::iterator _stackTopPtr;
	cat::Stack<uint64_t> _programmCounterStack;
	cat::SharedPtr<cat::DynArray<Value>> _globals;

	std::unordered_map<size_t, tsl::ordered_set<cat::WeakPtr<cat::DynArray<Value>>>> _unusedHeap;
	tsl::ordered_set<cat::WeakPtr<cat::DynArray<Value>>> _usedHeap;
	std::deque<cat::DynArray<Value>> _heap;

	const bool _printStacklayout = false;

protected:

	Reference allocBlock(int64_t size);
	void deallocBlock(Reference reference);

	void nullPointerException();
};

}
#endif // INSTRUCTIONEXECUTER_H

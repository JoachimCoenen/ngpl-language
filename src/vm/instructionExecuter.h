#ifndef INSTRUCTIONEXECUTER_H
#define INSTRUCTIONEXECUTER_H

#include "instruction.h"
#include "value.h"

#include "cat_utils.h"
#include "cat_stack.h"

#include <vector>

namespace ngpl {

class ExecutionError : public cat::Exception
{
	// TODO: maybe add token argument to SyntaxError?
public:
	ExecutionError(const std::string& message, const Instruction& instruction);

	const Instruction& instruction() const { return _instruction; }
	const std::string& rawMessage() const { return _rawMessage; }

protected:
	Instruction _instruction;
	std::string _rawMessage;
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

	std::vector<Value> _variables;
	cat::Stack<Value> _temporaryStack;
	cat::Stack<uint64_t> _programmCounterStack;
	//cat::Stack<uint64_t> _stackFramePtr;
};

}
#endif // INSTRUCTIONEXECUTER_H

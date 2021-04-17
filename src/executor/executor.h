#ifndef EXECUTOR_H
#define EXECUTOR_H

#include "stackFrame.h"

#include "callFrameStack.h"

namespace ngpl::executor {

using namespace cat;

class Executor
{
public:
	Executor();

	void executeInstruction();

	inline StackFrame& currentFrame() { return _callStack.current(); }

protected:
	void executeSimpleInstruction();

protected:
	CallStack _callStack;


};

}

#endif // EXECUTOR_H

#ifndef CALLFRAMESTACK_H
#define CALLFRAMESTACK_H

#include "stackFrame.h"

#include "cat_stack.h"
#include "cat_exception.h"

#include <stdexcept>


namespace ngpl::executor {

using namespace cat;

class BadStackError: Exception {
public:
	BadStackError(const cat::String& message)
		: Exception(message)
	{}
};

class CallStack
{
public:
	CallStack();


private:
	Stack<StackFrame> _stackFrames;

public:
	bool empty() const {
		return _stackFrames.empty();
	}

	size_t size() const {
		return _stackFrames.size();
	}

	StackFrame& current() {
		return _stackFrames.peek();
	}

	const StackFrame& current() const {
		return _stackFrames.peek();
	}

	StackFrame pop() {
		if (size() <= 1) {
			throw BadStackError("The last stack frame cannot be removed. Use CallStack::clear() to clear the call stack.");
		}
		return _stackFrames.pop();
	}

	void push() {
		_stackFrames.push(StackFrame());
	}

	void clear() {
		_stackFrames.clear();
		_stackFrames.push(StackFrame());
	}

protected:
	inline void assertAtLeastOneStackFrame() {

	}
};

}

#endif // CALLFRAMESTACK_H

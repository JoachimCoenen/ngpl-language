#include "callFrameStack.h"

namespace ngpl::executor {

CallStack::CallStack()
	: _stackFrames()
{
	_stackFrames.push(StackFrame());
}

}

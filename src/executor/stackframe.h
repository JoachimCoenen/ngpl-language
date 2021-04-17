#ifndef STACKFRAME_H
#define STACKFRAME_H

#include "vm/value.h"

#include "cat_DynArray.h"
#include "cat_stack.h"

namespace ngpl::executor {


class StackFrame {
public:
	StackFrame();

protected:
	cat::DynArray<Value> _variables;
	cat::Stack<Value> _temporaryStack;

};

}

#endif // STACKFRAME_H

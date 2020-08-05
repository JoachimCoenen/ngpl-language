#include "function.h"

#include "ranges.h"

namespace ngpl {

int32_t FunctionBase::stackDelta() const
{
	return _returnType->fixedSize() - argumentsStackSize();
}

int32_t FunctionBase::argumentsStackSize() const
{
	return cat::range(_signature.argumentTypes())
			.map_c(LAMBDA(v){ return v->fixedSize(); })
			.join() + (isMethod() ? 1 : 0);
}

cat::WriterObjectABC& Function::print(cat::WriterObjectABC& s) const {
	s+= cat::nlIndent;
	s += "func ";
	s += asCodeString();
	s.incIndent();
	InstructionsContainer::print(s);
	s.decIndent();
	return s;
}


}

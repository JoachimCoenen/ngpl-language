#include "function.h"

#include "ranges.h"

namespace ngpl {

int32_t FunctionBase::stackDelta() const
{
	return cat::range(_signature.argumentTypes())
			.map_c(LAMBDA(v){ return -(v->fixedSize()); })
			.join() + _returnType->fixedSize()
			 - (isMethod() ? 1 : 0);
}

cat::WriterObjectABC& Function::print(cat::WriterObjectABC& s) const {
	s += "func ";
	s += asCodeString();
	s.incIndent();
	InstructionsContainer::print(s);
	s.decIndent();
	return s;
}


}

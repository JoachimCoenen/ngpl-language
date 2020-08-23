#include "function.h"

#include "ranges.h"

namespace ngpl {

int32_t FunctionBase::stackDelta() const
{
	return _returnType->fixedSize() - argumentsStackSize();
}

int32_t FunctionBase::argumentsStackSize() const
{
	// next line is commented out, bc. 'self' variable is temporarely passed as a STACK_VAL and not a STACK_REF:
	//return cat::range(_signature.argumentTypes())
	//		.map_c(LAMBDA(v){ return v->fixedSize(); })
	//		.join() + (isMethod() ? 1 : 0);

	return cat::range(_signature.argumentTypes())
			.map_c(LAMBDA(v){ return v->fixedSize(); })
			.join();
}

cat::WriterObjectABC& Function::print(cat::WriterObjectABC& s) const {
	s+= cat::nlIndent;
	s += "func ";
	s += asCodeString();
	s.incIndent();
	body().print(s);
	s.decIndent();
	return s;
}

void Function::recalculteSideEffects()
{
	_hasSideEffect = body().hasSideEffect();
}


}

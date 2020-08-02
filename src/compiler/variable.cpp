#include "variable.h"

namespace ngpl {

Variable::Variable()
{}

Variable::Variable(const TypeCWeakPtr& type, Address address, ReferenceMode referenceMode, bool isOwning, bool isConst, bool isTemporary)
	: _type(type),
	  _address(address),
	  _referenceMode(referenceMode),
	  _isOwning(isOwning),
	  _isConst(isConst),
	_isTemporary(isTemporary)
{}

uint64_t Variable::fixedSize() const
{
//	STACK_VAL,  // _address is relAddr of value in stack
//	STACK_REF,  // _address is relAddr of actual relAddr of value in stack
//	HEAP_REF,   // _address is relAddr of addr of value in heap
	switch (referenceMode()) {
	case ReferenceMode::STACK_VAL:
		return type()->fixedSize();
	case ReferenceMode::STACK_REF:
		return 1;
	case ReferenceMode::HEAP_VAL:
		return 0;
	case ReferenceMode::HEAP_REF:
		return 1;
	}
	return 999999999; // Should never ever get here...
}

}

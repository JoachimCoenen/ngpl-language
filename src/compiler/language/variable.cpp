#include "variable.h"

namespace ngpl {

Variable::Variable(TypeReference&& type, Address address, ReferenceMode referenceMode, bool isOwning, bool isConst, bool isTemporary, Address fixedOffset)
	: _type(std::move(type)),
	  _address(address),
	  _referenceMode(referenceMode),
	  _isOwning(isOwning),
	  _isConst(isConst),
	  _isTemporary(isTemporary),
	  _fixedOffset(fixedOffset)
{}

uint64_t Variable::fixedSize() const
{
//	STACK_VAL,  // _address is relAddr of value in stack
//	HEAP_REF,   // _address is relAddr of addr of value in heap
	switch (referenceMode()) {
	case ReferenceMode::STACK_VAL:
		return type().fixedSize();
	case ReferenceMode::HEAP_VAL:
		return 0;
	}
	return 999999999; // Should never ever get here...
}

}

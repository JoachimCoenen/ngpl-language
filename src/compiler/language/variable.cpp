#include "variable.h"

#include "util/debug.h"

namespace ngpl {

Variable::Variable(TypeReference&& type, FrameAddr address, ReferenceMode referenceMode, bool isOwning, bool isConst, bool isTemporary, Address fixedOffset)
	: _type(std::move(type)),
	  _address(Address(address)),
	  _fixedOffset(fixedOffset),
	  _referenceMode(referenceMode),
	  _isOwning(isOwning),
	  _isConst(isConst),
	  _isTemporary(isTemporary)
{
	NGPL_ASSERT(isRepresentedByReference() ? true : _fixedOffset == 0);
}

Variable::Variable(const TypeReference& type, FrameAddr address, ReferenceMode referenceMode, bool isOwning, bool isConst, bool isTemporary, Address fixedOffset)
	: Variable(TypeReference{type}, address, referenceMode, isOwning, isConst, isTemporary, fixedOffset)
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

IndirectAccess::IndirectAccess(Variable&& variable)
	: _variable(std::move(variable)),
	  _address(0),
	  _isTemporary(false),
	  _isIndirect(false)
{}

IndirectAccess::IndirectAccess(FrameAddr address, bool isTemporary, Variable&& variable)
	: _variable(std::move(variable)),
	  _address(address),
	  _isTemporary(isTemporary),
	  _isIndirect(true)
{
	//NGPL_ASSERT2(_variable.isReference(), "indirect access (with isIndirect==true) requires that the variable is actually a pointer!");
}

}

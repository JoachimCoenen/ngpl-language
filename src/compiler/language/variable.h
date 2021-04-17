#ifndef VARIABLE_H
#define VARIABLE_H

#include "type.h"
#include "typeReference.h"
#include "vm/value.h"

#include "util/types.h"

namespace ngpl {

enum class ReferenceMode {
	STACK_VAL,  // _address is relAddr of value in stack					// aka "a value"
	HEAP_VAL,   // _address is addr of value in heap						// aka " a value in heap "
	//HEAP_REF,   // _address is relAddr of addr of value in heap				// aka "pointer to a value in heap "
};

PTRS_FOR_CLASS(Scope)

PTRS_FOR_CLASS(Variable)
class Variable
{
public:
	/**
	 * @brief Variable
	 * @param type
	 * @param address
	 * @param referenceMode
	 * @param isOwning
	 * @param isConst
	 * @param isTemporary if variable is Temporary, address doesnt matter at time of usage; it MUST be at the top of the stack and will be consumed
	 */
	Variable(TypeReference&& type, Address address, ReferenceMode referenceMode, bool isOwning, bool isConst, bool isTemporary=false, Address fixedOffset=0);

	const TypeReference& type() const { return _type; }
	Address address() const { return _address; }

	ReferenceMode referenceMode() const { return _referenceMode; }

	bool isReference() const { return _type.isReference(); }
	bool isConst() const { return _isConst; }
	bool isOwning() const { return _isOwning; }
	bool isTemporary() const { return _isTemporary; }

	Address fixedOffset() const { return _fixedOffset; } // only for reference variables, offset relaive to the reference target

	uint64_t fixedSize() const;
protected:
	TypeReference _type; // do not modiyfy the TypeReference!
	Address _address;  // where do I find the value / reference?
	ReferenceMode _referenceMode;  // stack or heap?
	bool _isOwning;  // ?
	bool _isConst;  // is const
	bool _isTemporary;  // to be consumed by the next usage
	Address _fixedOffset; // only for reference variables, offset relaive to the reference target

	friend class Scope;
};

}
#endif // VARIABLE_H






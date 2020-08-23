#ifndef VARIABLE_H
#define VARIABLE_H

#include "type.h"
#include "vm/value.h"

#include "util/types.h"

namespace ngpl {

enum class ReferenceMode {
	STACK_VAL,  // _address is relAddr of value in stack					// aka "a value"
	STACK_REF,  // _address is relAddr of actual relAddr of value in stack	// aka "reference to a value in stack"
	HEAP_VAL,   // _address is addr of value in heap						// aka " a value in heap "
	HEAP_REF,   // _address is relAddr of addr of value in heap				// aka "pointer to a value in heap "
};

PTRS_FOR_CLASS(Scope)

PTRS_FOR_CLASS(Variable)
class Variable
{
public:
	Variable();
	/**
	 * @brief Variable
	 * @param type
	 * @param address
	 * @param referenceMode
	 * @param isOwning
	 * @param isConst
	 * @param isTemporary if variable is Temporary, address doesnt matter at time of usage; it MUST be at the top of the stack and will be consumed
	 */
	Variable(const TypeCWeakPtr& type, Address address, ReferenceMode referenceMode, bool isOwning, bool isConst, bool isTemporary=false);

	const TypeCWeakPtr& type() const { return _type; }
	Address address() const { return _address; }
	ReferenceMode referenceMode() const { return _referenceMode; }
	bool isConst() const { return _isConst; }
	bool isOwning() const { return _isOwning; }
	bool isTemporary() const { return _isTemporary; }

	uint64_t fixedSize() const;
protected:
	TypeCWeakPtr _type;
	Address _address;
	ReferenceMode _referenceMode;
	bool _isOwning;
	bool _isConst;
	bool _isTemporary;

	friend class Scope;
};

}
#endif // VARIABLE_H






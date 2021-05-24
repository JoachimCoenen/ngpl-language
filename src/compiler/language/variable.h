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

PTRS_FOR_CLASS(Scope);

PTRS_FOR_CLASS(Variable);
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
	Variable(TypeReference&& type, FrameAddr address, ReferenceMode referenceMode, bool isOwning, bool isConst, bool isTemporary=false, Address fixedOffset=0);

	/**
	 * @brief Variable
	 * @param type
	 * @param address
	 * @param referenceMode
	 * @param isOwning
	 * @param isConst
	 * @param isTemporary if variable is Temporary, address doesnt matter at time of usage; it MUST be at the top of the stack and will be consumed
	 */
	Variable(const TypeReference& type, FrameAddr address, ReferenceMode referenceMode, bool isOwning, bool isConst, bool isTemporary=false, Address fixedOffset=0);

	const TypeReference& type() const { return _type; }

	/**
	 * @brief the position of the value/reference.
	 * @return
	 * The address is:
	 * if referenceMode() == STACK_VAL:
	 *     in callFrame-space, i.e. relative to the start of the current call frame
	 * if referenceMode() == HEAP_VAL:
	 *     or on the heap ()
	 */
	FrameAddr address() const { return _address; }

	ReferenceMode referenceMode() const { return _referenceMode; }

	inline int pointerDepth() const { return _type.pointerDepth(); }
	inline bool isPointer() const { return _type.isPointer(); }
	inline bool isReference() const { return _type.isReference(); }
	inline bool isClass() const { return _type.isClass(); }
	inline bool isReferenceOrClass() const { return _type.isReferenceOrClass(); }
	inline bool isRepresentedByReference() const { return _type.isRepresentedByReference(); }

	inline bool isConst() const { return _isConst; }
	inline bool isOwning() const { return _isOwning; }
	inline bool isTemporary() const { return _isTemporary; }

	inline Address fixedOffset() const { return _fixedOffset; } // only for reference variables, offset relaive to the reference target

	uint64_t fixedSize() const;
protected:
	TypeReference _type; /// do not modiyfy the TypeReference!
	FrameAddr _address;  /// where do I find the value / pointer?
	Address _fixedOffset; /// only for reference variables, offset relaive to the reference target
	ReferenceMode _referenceMode;  /// stack or heap?
	bool _isOwning;  /// ?
	bool _isConst;  /// is const
	bool _isTemporary;  /// to be consumed by the next usage

	friend class Scope;
};


PTRS_FOR_CLASS(IndirectAccess);
class IndirectAccess
{
public:
	explicit IndirectAccess(Variable&& variable);
	IndirectAccess(FrameAddr address, bool isTemporary, Variable&& variable);

	bool isIndirect() const { return _isIndirect; }
	FrameAddr address() const { return _address; }
	bool isTemporary() const { return _isTemporary; }
	const Variable& variable() const { return _variable; }
protected:
	Variable _variable;
	FrameAddr _address;  /// where do I find the reference?
	bool _isTemporary;  /// to be consumed by the next usage
	bool _isIndirect;  ///
	//Address _offset;
};

}

#endif // VARIABLE_H






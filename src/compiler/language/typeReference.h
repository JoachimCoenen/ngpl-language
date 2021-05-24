#ifndef NGPL_TYPEREFERENCE_H
#define NGPL_TYPEREFERENCE_H

#include "util/debug.h"

#include "type.h"

namespace ngpl {

//PTRS_FOR_CLASS(TypeReference);
struct TypeReference {
private:
	TypeCWeakPtr _baseType;
	std::vector<TypeReference> _arguments;
	int _pointerDepth;
	bool _isReference;

public:
	TypeReference(TypeCWeakPtr baseType, std::vector<TypeReference>&& arguments, int pointerDepth, bool isReference);
	inline TypeReference(TypeCWeakPtr baseType,                                  int pointerDepth, bool isReference = false)
	   : TypeReference(baseType, {}, pointerDepth, isReference)
	{}
//	inline TypeReference(const TypeReference& typeRef,                                  int pointerDepth, bool isReference)
//		: TypeReference(typeRef.baseType(), std::vector(typeRef.arguments()), pointerDepth, isReference)
//	{}

	TypeReference asReference() const {
		return TypeReference( baseType(), std::vector(arguments()), pointerDepth(), true);
	}

	TypeReference asValue() const {
		return TypeReference( baseType(), std::vector(arguments()), pointerDepth(), false);
	}

	TypeReference asPointerLess() const {
		NGPL_ASSERT(not _isReference);
		return TypeReference( baseType(), std::vector(arguments()), pointerDepth() - 1, _isReference);
	}


	TypeReference asPointerMore() const {
		NGPL_ASSERT(not _isReference);
		return TypeReference( baseType(), std::vector(arguments()), pointerDepth() + 1, _isReference);
	}

	TypeCWeakPtr baseType() const noexcept { return _baseType; }
	const std::vector<TypeReference>& arguments() const { return _arguments; }
	int pointerDepth() const noexcept { return _pointerDepth; }
	bool isPointer() const noexcept { return pointerDepth() > 0; }
	bool isReference() const noexcept { return _isReference; }
	bool isClass() const noexcept { return _baseType->isClass() and not isPointer(); }
	bool isReferenceOrClass() const noexcept { return isReference() or isClass(); }
	bool isRepresentedByReference() const noexcept { return isReference() or isPointer() or isClass(); }

	bool isGeneric() const noexcept { return not _arguments.empty(); }

	bool isAssignableTo(const TypeReference& destination) const;

	uint64_t fixedSize() const;
	ScopeCWeakPtr scope() { return _baseType->scope(); }
	ScopeCWeakPtr scope() const { return _baseType->scope(); }

	cat::String asCodeString() const;
	cat::String asQualifiedCodeString() const;

};





bool operator ==(const TypeReference& lhs, const TypeReference& rhs);
inline bool operator !=(const TypeReference& lhs, const TypeReference& rhs)
{
	return not (lhs == rhs);
}

cat::WriterObjectABC& operator +=(cat::WriterObjectABC& s, const TypeReference& v);

} // namespace ngpl

template<>
struct std::hash<ngpl::TypeReference> {
	size_t operator()(const ngpl::TypeReference& v) const noexcept {
		size_t result = 0xd6a7f393910ec6a9; // a random number
		result = cat::combineHashes(result, cat::hash(v.baseType()));
		result = cat::combineHashes(result, cat::hash(v.arguments()));
		result = cat::combineHashes(result, cat::hash(v.pointerDepth()));
		result = cat::combineHashes(result, cat::hash(v.isReference()));
		return result;
	}
};
#endif // NGPL_TYPEREFERENCE_H

#ifndef NGPL_TYPEREFERENCE_H
#define NGPL_TYPEREFERENCE_H


#include "type.h"

namespace ngpl {

//PTRS_FOR_CLASS(TypeReference);
struct TypeReference {
private:
	TypeCWeakPtr _baseType;
	std::vector<TypeReference> _arguments;
	bool _isPointer;

public:
	TypeReference(TypeCWeakPtr baseType, std::vector<TypeReference>&& arguments, bool isPointer);

	inline TypeReference(TypeCWeakPtr baseType, bool isPointer = false)
		: TypeReference(baseType, {}, isPointer)
	{}

	inline TypeReference(const TypeReference& typeRef, bool isPointer)
		: TypeReference(typeRef.baseType(), std::vector(typeRef.arguments()), isPointer)
	{}

	TypeReference asPointer() const {
		return TypeReference( baseType(), std::vector(arguments()), true);
	}

	TypeReference asValue() const {
		return TypeReference( baseType(), std::vector(arguments()), false);
	}

	TypeCWeakPtr baseType() const noexcept { return _baseType; }
	const std::vector<TypeReference>& arguments() const { return _arguments; }
	bool isPointer() const noexcept { return _isPointer or _baseType->isClass(); }
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
		result = cat::combineHashes(result, cat::hash(v.isPointer()));
		return result;
	}
};
#endif // NGPL_TYPEREFERENCE_H

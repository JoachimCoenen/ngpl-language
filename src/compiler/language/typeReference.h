#ifndef NGPL_TYPEREFERENCE_H
#define NGPL_TYPEREFERENCE_H


#include "type.h"

namespace ngpl {

//PTRS_FOR_CLASS(TypeReference);
struct TypeReference {
private:
	TypeCWeakPtr _baseType;
	std::vector<TypeReference> _arguments;
	bool _isReference;

public:
	TypeReference(TypeCWeakPtr baseType, std::vector<TypeReference>&& arguments, bool isReference);

//	inline TypeReference(TypeCDynamicPtr&& baseType, bool isReference = false)
//		: TypeReference(std::move(baseType), {}, isReference)
//	{}
	inline TypeReference(const TypeCWeakPtr& baseType, bool isReference = false)
		: TypeReference(baseType, {}, isReference)
	{}

	TypeReference asReference() {
		return TypeReference( baseType(), std::vector<TypeReference>{arguments()}, true);
	}

	TypeCWeakPtr baseType() const noexcept { return _baseType; }
	const std::vector<TypeReference>& arguments() const { return _arguments; }
	bool isReference() const noexcept { return _isReference or _baseType->isClass(); }
	bool isGeneric() const noexcept { return not _arguments.empty(); }

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
		result = cat::combineHashes(result, cat::hash(v.isReference()));
		return result;
	}
};
#endif // NGPL_TYPEREFERENCE_H

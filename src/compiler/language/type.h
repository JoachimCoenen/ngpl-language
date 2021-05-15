#ifndef TYPE_H
#define TYPE_H

#include "util/types.h"
#include "intermediate/intermediateCode.h"
#include "member.h"

#include "cat_utils.h"
#include "cat_hash.h"
#include "cat_string.h"

namespace cat {
class WriterObjectABC;
}

namespace ngpl {


enum class TypeKind {
	BASIC, // Int, Floatm ...
	TUPLE_LIKE, // tuple, stuct, ...
	CLASS_LIKE,
	COMPLEX_ENUM,
};

cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const TypeKind& v);



PTRS_FOR_CLASS(Scope)

PTRS_FOR_CLASS(Type)
class Type: public IIntermediateCodePrintable, public Member
{
public:
	Type();
	Type(const cat::String& name, const cat::String& qualifier, uint64_t fixedSize, TypeKind typeKind, bool isFinished = false);
	//Type(cat::String&& name, uint64_t fixedSize, bool isBasic, bool isFinished = false);

	ScopeSharedPtr& scope() { return _scope; }
	ScopeCWeakPtr scope() const { return _scope.weak(); }
	void setScope(const ScopeSharedPtr& newScope) { _scope = newScope; }
	const intermediate::IntermediateCodeContainer& body() const { return _body; }
	intermediate::IntermediateCodeContainer& body() { return _body; }
	TypeKind typeKind() const { return _typeKind; }
	bool isBasic() const { return _typeKind == TypeKind::BASIC; }
	bool isTupleLike() const { return _typeKind == TypeKind::TUPLE_LIKE; }
	bool isClass() const { return _typeKind == TypeKind::CLASS_LIKE; }
	bool isAssignableTo(const Type& destination) const;


	uint64_t fixedSize() const { return _fixedSize; }

	bool isFinished() const { return _isFinished; }
	void finish();

	cat::WriterObjectABC& print(cat::WriterObjectABC& s) const override;

protected:
	ScopeSharedPtr _scope;
	intermediate::IntermediateCodeContainer _body;
	uint64_t _fixedSize;
	TypeKind _typeKind;
	bool _isHeapAllocated;
	bool _isFinished;
};

}

template <>
struct std::hash<ngpl::Type> {
	size_t operator()(const ngpl::Type& v) const noexcept {
		return cat::combineHashes(
				cat::hash(v.qualifier()), cat::hash(v.name())
		);
	}

};
#endif // TYPE_H














#ifndef TYPE_H
#define TYPE_H

#include "util/types.h"
#include "compiler/intermediate/intermediateCode.h"
#include "member.h"

#include "cat_utils.h"
#include "cat_hash.h"

#include <string>

namespace cat {
class WriterObjectABC;
}

namespace ngpl {

PTRS_FOR_CLASS(Scope)

PTRS_FOR_CLASS(Type)
class Type: public IIntermediateCodePrintable, public Member
{
public:
	Type();
	Type(const std::string& name, const std::string& qualifier, uint64_t fixedSize, bool isBasic, bool isFinished = false);
	//Type(std::string&& name, uint64_t fixedSize, bool isBasic, bool isFinished = false);

	ScopePtr& scope() { return _scope; }
	ScopeCWeakPtr scope() const { return _scope.getRaw(); }
	const intermediate::IntermediateCodeContainer& body() const { return _body; }
	intermediate::IntermediateCodeContainer& body() { return _body; }
	bool isBasic() const { return _isBasic; }
	uint64_t& fixedSize() { return _fixedSize; }
	uint64_t fixedSize() const { return _fixedSize; }

	bool isFinished() const { return _isFinished; }
	bool finish() { return _isFinished = true; }

	cat::WriterObjectABC& print(cat::WriterObjectABC& s) const override;

protected:
	ScopePtr _scope;
	intermediate::IntermediateCodeContainer _body;
	uint64_t _fixedSize;
	bool _isBasic;
	bool _isFinished;
};

inline cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const Type& v);

PTRS_FOR_STRUCT(FunctionSignature)
struct FunctionSignature
{
public:
	FunctionSignature();
	FunctionSignature(
	//std::string&& name,
	//TypeCWeakPtr&& returnType,
	std::vector<TypeCWeakPtr>&& argumentTypes
	);

	//const std::string& name() const { return _name; }

	//const TypeCWeakPtr returnType() const { return _returnType; }
	const std::vector<TypeCWeakPtr>& argumentTypes() const { return _argumentTypes; }
	uint_fast16_t argumentCount() const { return _argumentTypes.size(); }

	std::string asCodeString() const;
	std::string asQualifiedCodeString() const;


protected:
	//std::string _name;
	//TypeCWeakPtr _returnType;
	std::vector<TypeCWeakPtr> _argumentTypes;
};

inline bool operator == (const FunctionSignature& lhs, const FunctionSignature& rhs) {
	return true
		//and lhs.name() == rhs.name()
		//and lhs.returnType() == rhs.returnType()
		and lhs.argumentTypes() == rhs.argumentTypes();
}

inline cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const FunctionSignature& v);

}

template <>
struct std::hash<ngpl::Type> {
	size_t operator()(const ngpl::Type& v) const noexcept {
	return cat::hash(v.name());
	}

};

template <>
struct std::hash<ngpl::FunctionSignature> {
	size_t operator()(const ngpl::FunctionSignature& v) const noexcept {
	size_t result = 99;
	//result = cat::combineHashes(result, cat::hash(v.name()));
	//result = cat::combineHashes(result, cat::hash(v.returnType()));
	result = cat::combineHashes(result, cat::hash(v.argumentTypes()));
	return result;
	}

};
#endif // TYPE_H














#ifndef FUNCTION_H
#define FUNCTION_H

#include "intermediate/intermediateCode.h"
#include "member.h"
#include "type.h"
#include "typeReference.h"
#include "vm/value.h"
#include "vm/callStack.h"
#include "vm/vm_util.h"
#include "util/instructionID.h"
#include "util/types.h"

#include "cat_DynArray.h"
#include "cat_stack.h"
#include "toStringUtils.h"

#include <vector>
#include <functional>

namespace ngpl {

PTRS_FOR_STRUCT(CallArgTypes)
struct CallArgTypes
{
	cat::DynArray<TypeReference> types;

	CallArgTypes(cat::DynArray<TypeReference>&& a)
		: types(std::move(a))
	{}

	cat::String asCodeString() const;
	cat::String asQualifiedCodeString() const;
};

PTRS_FOR_STRUCT(FunctionSignature)
struct FunctionSignature
{
public:
	FunctionSignature();
	FunctionSignature(//cat::String&& name,
	cat::DynArray<TypeReference>&& parameterTypes,
	TypeReference&& returnType
	);

	//const cat::String& name() const { return _name; }

	const cat::DynArray<TypeReference>& parameterTypes() const { return _parameterTypes; }
	uint_fast16_t parameterCount() const { return _parameterTypes.size(); }
	const TypeReference& returnType() const { return _returnType; }

	bool isCallableWith(const CallArgTypes& argTypes) const;

	cat::String asCodeString() const;
	cat::String asQualifiedCodeString() const;


protected:
	//cat::String _name;
	cat::DynArray<TypeReference> _parameterTypes;
	TypeReference _returnType;
};

inline bool operator == (const FunctionSignature& lhs, const FunctionSignature& rhs) {
	return true
		//and lhs.name() == rhs.name()
		//and lhs.returnType() == rhs.returnType()
		and lhs.parameterTypes() == rhs.parameterTypes();
}

inline cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const FunctionSignature& v);


using CallStack = CallStack;  // = cat::Stack<Value>

PTRS_FOR_CLASS(FunctionBase);
class FunctionBase : public Member
{
public:
	FunctionBase(
		const cat::String& name,
		const cat::String& qualifier,
		FunctionSignature&& signature,
		const TypeCWeakPtr& selfType,
		bool hasSideEffect
	)
	: Member(name, qualifier),
	  _signature(std::move(signature)),
	  _selfType(selfType),
	  _hasSideEffect(hasSideEffect)
	{}

	virtual ~FunctionBase() {};

	uint_fast16_t parameterCount() const { return _signature.parameterCount(); }
	const FunctionSignature& signature() const { return _signature; }
	const TypeReference& returnType() const { return _signature.returnType(); }
	const TypeCWeakPtr selfType() const { return _selfType; }
	bool isMethod() const { return _selfType != nullptr; }
	bool hasSideEffect() const { return _hasSideEffect; }

	int32_t stackDelta() const;
	int32_t argumentsStackSize() const;

	bool isCallableWith(const CallArgTypes& argTypes) const;

	cat::String asCodeString() const override;
	cat::String asQualifiedCodeString() const override;
protected:
	FunctionSignature _signature;
	TypeCWeakPtr _selfType;
	bool _hasSideEffect;
};


PTRS_FOR_CLASS(FunctionOverloads)
class FunctionOverloads
{
private:
	std::vector<FunctionBasePtr> _overloads;
public:
	FunctionOverloads();

	void add(FunctionBasePtr&& function);

	bool canAddOverload(const FunctionSignature& signature, cat::String& reasonOut) const;
	FunctionBaseCWeakPtr tryGetOverload(const CallArgTypes& argTypes) const;
	//FunctionBaseCWeakPtr tryGetOverload(const FunctionSignature& signature) const;
	bool hasOverload(const CallArgTypes& argTypes) const;

	//ITERATORS
	auto begin() noexcept { return std::begin(_overloads); }
	auto end()   noexcept { return std::end(_overloads); }

	auto begin() const noexcept { return std::cbegin(_overloads); }
	auto end()   const noexcept { return std::cend(_overloads); }

	auto cbegin() const noexcept { return std::cbegin(_overloads); }
	auto cend()   const noexcept { return std::cend(_overloads); }

};


struct FunctionOverloadsBuilder {
	FunctionOverloads fo;

	FunctionOverloadsBuilder& add(FunctionBasePtr&& function);

	FunctionOverloads build();

};


using BuiltinFunctionBody = std::function<Value(CallStack&)>;

PTRS_FOR_CLASS(BuiltinFunction);
class BuiltinFunction: public FunctionBase
{
public:
	BuiltinFunction(
		const cat::String& name,
		const cat::String& qualifier,
		FunctionSignature&& signature,
		bool hasSideEffect,
		const BuiltinFunctionBody& body,
		const std::optional<std::vector<InstructionID>>& instructions
	)
		: FunctionBase(name, qualifier, std::move(signature), nullptr, hasSideEffect),
		  _body(body),
		  _instructions(instructions)
	{}

	Value call(CallStack& values) const {
		return _body((values));
	}

//protected:
	BuiltinFunctionBody _body;
	std::optional<std::vector<InstructionID>> _instructions;

};


PTRS_FOR_CLASS(Function);
class Function: public FunctionBase, public IIntermediateCodePrintable//, public InstructionsContainer
{
public:
	using Body = intermediate::IntermediateCodeContainer;
public:
	Function(
	const cat::String& name,
	const cat::String& qualifier,
	FunctionSignature&& signature,
	const TypeCWeakPtr& selfType,
	bool hasSideEffect)
		: FunctionBase(name, qualifier, std::move(signature), selfType, hasSideEffect)
	{}

	Function(
	const cat::String& name,
	const cat::String& qualifier,
	FunctionSignature&& signature,
	const TypeCWeakPtr& selfType,
	bool hasSideEffect,
	Body&& body)
		: FunctionBase(name, qualifier, std::move(signature), selfType, hasSideEffect),
		  _body(std::move(body))
	{}

	const Body& body() const { return _body; }
	Body& body() { return _body; }
	//const cat::String& name() const { return _name; }
	//const InstructionPos& body() const { return _body; }

//    cat::String asCodeString() const {
//        return cat::SW() << _name << "(" << _signature.asCodeString() << ") -> " << returnType()->asCodeString();
//    }
	cat::WriterObjectABC& print(cat::WriterObjectABC& s) const override;

	void recalculteSideEffects();
protected:
	//cat::String _name;
	//InstructionPos _body;
	Body _body;

};

}

template <>
struct std::hash<ngpl::FunctionSignature> {
	size_t operator()(const ngpl::FunctionSignature& v) const noexcept {
	size_t result = 99;
	//result = cat::combineHashes(result, cat::hash(v.name()));
	//result = cat::combineHashes(result, cat::hash(v.returnType()));
	result = cat::combineHashes(result, cat::hash(v.parameterTypes()));
	return result;
	}

};
#endif // FUNCTION_H

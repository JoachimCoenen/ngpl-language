#ifndef FUNCTION_H
#define FUNCTION_H

#include "compiler/intermediate/intermediateCode.h"
#include "member.h"
#include "type.h"
#include "vm/value.h"
#include "vm/vm_util.h"
#include "util/instructionID.h"
#include "util/types.h"

#include "cat_stack.h"
#include "toStringUtils.h"

#include <vector>
#include <functional>

namespace ngpl {


PTRS_FOR_CLASS(FunctionBase);
class FunctionBase : public Member
{
public:
	FunctionBase(
		const std::string& name,
		const std::string& qualifier,
		const FunctionSignature& signature,
		const TypeCWeakPtr& returnType,
		const TypeCWeakPtr& selfType,
		bool hasSideEffect
	)
	: Member(name, qualifier),
	  _signature(signature),
	  _returnType(returnType),
	  _selfType(selfType),
	  _hasSideEffect(hasSideEffect)
	{}

	virtual ~FunctionBase() {};

	uint_fast16_t argumentCount() const { return _signature.argumentCount(); }
	const FunctionSignature& signature() const { return _signature; }
	const TypeCWeakPtr returnType() const { return _returnType; }
	const TypeCWeakPtr selfType() const { return _selfType; }
	bool isMethod() const { return _selfType != nullptr; }
	bool hasSideEffect() const { return _hasSideEffect; }

	int32_t stackDelta() const;
	int32_t argumentsStackSize() const;

	std::string asCodeString() const override {
		return cat::SW() << Member::asCodeString() << "(" << _signature.asQualifiedCodeString() << ") -> " << returnType()->asQualifiedCodeString();
	}

	std::string asQualifiedCodeString() const override {
		return cat::SW() << Member::asQualifiedCodeString() << "(" << _signature.asQualifiedCodeString() << ") -> " << returnType()->asQualifiedCodeString();
	}
protected:
	FunctionSignature _signature;
	TypeCWeakPtr _returnType;
	TypeCWeakPtr _selfType;
	bool _hasSideEffect;
};



PTRS_FOR_CLASS(BuiltinFunction);
class BuiltinFunction: public FunctionBase
{
public:
	BuiltinFunction(
		const std::string& name,
		const std::string& qualifier,
		const FunctionSignature& signature,
		const TypeCWeakPtr& returnType,
		bool hasSideEffect,
		const std::function<Value(cat::Stack<Value>&)>& body,
		const std::optional<std::vector<InstructionID>>& instructions
	)
		: FunctionBase(name, qualifier, signature, returnType, nullptr, hasSideEffect),
		  _body(body),
		  _instructions(instructions)
	{}

	Value call(cat::Stack<Value>& values) const {
		return _body((values));
	}

//protected:
	std::function<Value(cat::Stack<Value>&)> _body;
	std::optional<std::vector<InstructionID>> _instructions;

};


PTRS_FOR_CLASS(Function);
class Function: public FunctionBase, public IIntermediateCodePrintable//, public InstructionsContainer
{
public:
	Function(
	const std::string& name,
	const std::string& qualifier,
	const FunctionSignature& signature,
	const TypeCWeakPtr& returnType,
	const TypeCWeakPtr& selfType,
	bool hasSideEffect
	//const InstructionPos body
	)
	: FunctionBase(name, qualifier, signature, returnType, selfType, hasSideEffect)//,
	 // _body(body)
	{}

	const intermediate::IntermediateCodeContainer& body() const { return _body; }
	intermediate::IntermediateCodeContainer& body() { return _body; }
	//const std::string& name() const { return _name; }
	//const InstructionPos& body() const { return _body; }

//    std::string asCodeString() const {
//        return cat::SW() << _name << "(" << _signature.asCodeString() << ") -> " << returnType()->asCodeString();
//    }
	cat::WriterObjectABC& print(cat::WriterObjectABC& s) const override;

	void recalculteSideEffects();
protected:
	//std::string _name;
	//InstructionPos _body;
	intermediate::IntermediateCodeContainer _body;

};


}
#endif // FUNCTION_H

#ifndef FUNCTION_H
#define FUNCTION_H

#include "instructionsContainer.h"
#include "member.h"
#include "type.h"
#include "../vm/instruction.h"
#include "../vm/value.h"
#include "../vm/vm_util.h"
#include "../util/types.h"

#include "cat_stack.h"
#include "toStringUtils.h"

#include <vector>
#include <functional>

namespace ngpl {


PTRS_FOR_CLASS(FunctionBase)
class FunctionBase : public Member
{
public:
	FunctionBase(
		const std::string& name,
		const std::string& qualifier,
		const FunctionSignature& signature,
		const TypeCWeakPtr& returnType,
		bool isMethod
	)
	: Member(name, qualifier),
	  _signature(signature),
	  _returnType(std::move(returnType)),
	  _isMethod(isMethod)
	{}

	virtual ~FunctionBase() {};

	uint_fast16_t argumentCount() const { return _signature.argumentCount(); }
	const FunctionSignature& signature() const { return _signature; }
	const TypeCWeakPtr returnType() const { return _returnType; }
	bool isMethod() const { return _isMethod; }

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
	bool _isMethod;

};



PTRS_FOR_CLASS(BuiltinFunction)
class BuiltinFunction: public FunctionBase
{
public:
	BuiltinFunction(
		const std::string& name,
		const std::string& qualifier,
		const FunctionSignature& signature,
		const TypeCWeakPtr& returnType,
		const std::function<Value(cat::Stack<Value>&)>& body,
		const std::optional<std::vector<InstructionID>>& instructions
	)
		: FunctionBase(name, qualifier, signature, std::move(returnType), false),
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


PTRS_FOR_CLASS(Function)
class Function: public FunctionBase, public InstructionsContainer
{
public:
	Function(
	const std::string& name,
	const std::string& qualifier,
	const FunctionSignature& signature,
	const TypeCWeakPtr& returnType,
	bool isMethod
	//const InstructionPos body
	)
	: FunctionBase(name, qualifier, signature, std::move(returnType), isMethod)//,
	 // _body(body)
	{}

	//const std::string& name() const { return _name; }
	//const InstructionPos& body() const { return _body; }

//    std::string asCodeString() const {
//        return cat::SW() << _name << "(" << _signature.asCodeString() << ") -> " << returnType()->asCodeString();
//    }
	cat::WriterObjectABC& print(cat::WriterObjectABC& s) const override;
protected:
	//std::string _name;
	//InstructionPos _body;

};


}
#endif // FUNCTION_H

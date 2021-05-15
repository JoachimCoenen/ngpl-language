#ifndef SCOPE_H
#define SCOPE_H

#include "vm/vm_util.h"

#include "type.h"
#include "function.h"
#include "variable.h"

#include <tsl/ordered_map.h>
#include <unordered_map>

namespace ngpl {

PTRS_FOR_CLASS(Scope);
class Scope: public IIntermediateCodePrintable {
public:
	Scope(FrameAddr salt) : salt(salt) {}

	 FrameAddr getFrameSize() const { return FrameAddr(salt); }

	std::unordered_map<cat::String, FunctionOverloads>& getFunctions() { return functions; }
	const std::unordered_map<cat::String, FunctionOverloads>& getFunctions() const { return functions; }

	std::unordered_map<cat::String, TypePtr>& getTypes() { return types; }
	const std::unordered_map<cat::String, TypePtr>& getTypes() const { return types; }

	const tsl::ordered_map<cat::String, VariableCPtr>& getVariables() const { return variables; }

	VariableCWeakPtr setVariable(const cat::String& name, VariablePtr&& variable);
	VariableCWeakPtr addVariable(const cat::String& name, VariablePtr&& variable);
	void addType(const cat::String& name, TypePtr&& type);
	FunctionBaseWeakPtr addFunction(const cat::String& name, FunctionBasePtr&& function);
	bool canAddFunction(const cat::String& name, const FunctionSignature& signature, cat::String& reasonOut) const;

	VariableCWeakPtr tryGetVariable(const cat::String& name) const;

	FunctionOverloadsCWeakPtr tryGetFunctionOverloads(const cat::String& name) const;
	FunctionBaseCWeakPtr tryGetFunction(const cat::String& name, const CallArgs& args) const;

	TypeWeakPtr tryGetType(const cat::String& name);
	TypeCWeakPtr tryGetType(const cat::String& name) const;

	cat::WriterObjectABC& print(cat::WriterObjectABC& s) const override final;

protected:
	FrameAddr salt;
	tsl::ordered_map<cat::String, VariableCPtr> variables;
	std::unordered_map<cat::String, FunctionOverloads> functions = {};
	std::unordered_map<cat::String, TypePtr> types;

};


}

#endif // SCOPE_H

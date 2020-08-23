#ifndef SCOPE_H
#define SCOPE_H

#include "vm/vm_util.h"

#include "type.h"
#include "function.h"
#include "variable.h"

#include <unordered_map>

namespace ngpl {

PTRS_FOR_CLASS(Scope)
class Scope: public IIntermediateCodePrintable {
public:
	Scope(Address&& salt) : salt(std::move(salt)) {}

	const Address& getFrameSize() const { return salt; }

	std::unordered_map<std::string, std::unordered_map<FunctionSignature, FunctionPtr>>& getFunctions() { return functions; }
	const std::unordered_map<std::string, std::unordered_map<FunctionSignature, FunctionPtr>>& getFunctions() const { return functions; }

	std::unordered_map<std::string, TypePtr>& getTypes() { return types; }
	const std::unordered_map<std::string, TypePtr>& getTypes() const { return types; }

	VariableCWeakPtr setVariable(const std::string& name, VariablePtr&& variable);
	VariableCWeakPtr addVariable(const std::string& name, VariablePtr&& variable);
	void addType(const std::string& name, TypePtr&& type);
	void addFunction(const std::string& name, FunctionSignature&& signature, FunctionPtr&& function);

	VariableCWeakPtr tryGetVariable(const std::string& name) const;

	FunctionBaseWeakPtr tryGetFunction(const std::string& name, const FunctionSignature& signature);
	FunctionBaseCWeakPtr tryGetFunction(const std::string& name, const FunctionSignature& signature) const;

	TypeWeakPtr tryGetType(const std::string& name);
	TypeCWeakPtr tryGetType(const std::string& name) const;

	cat::WriterObjectABC& print(cat::WriterObjectABC& s) const override final;

protected:
	Address salt;
	std::unordered_map<std::string, VariableCPtr> variables;
	std::unordered_map<std::string, std::unordered_map<FunctionSignature, FunctionPtr>> functions = {};
	std::unordered_map<std::string, TypePtr> types;

};


}

#endif // SCOPE_H

#include "scope.h"

#include "ranges.h"

namespace ngpl {

VariableCWeakPtr Scope::setVariable(const cat::String& name, VariablePtr&& variable)
{
	NGPL_ASSERT2(not variable->isTemporary(), "Variables, added to a Scope must not be temporary! (variable " + name + ")");
	NGPL_ASSERT2(variables.find(name) == variables.end(), "Variable " + name + " is already in variables!");
	salt = std::max(salt, variable->address() + Address(variable->fixedSize()));
	return (variables[name] = std::move(variable)).weak();
}

VariableCWeakPtr Scope::addVariable(const cat::String& name, VariablePtr&& variable)
{
	NGPL_ASSERT2(not variable->isTemporary(), "Variables, added to a Scope must not be temporary! (variable " + name + ")");
	NGPL_ASSERT2(variables.find(name) == variables.end(), "variable " + name + " is already in variables!");
	if (variable->referenceMode() != ReferenceMode::HEAP_VAL) {
		variable->_address = salt;
		salt += variable->fixedSize();
	}
	return (variables[name] = std::move(variable)).weak();
}

void Scope::addType(const cat::String& name, TypePtr&& type)
{
	NGPL_ASSERT2(types.find(name) == types.end(), "Type " + name + " is already in types!");
	NGPL_ASSERT2(type->name() == name, "Names don't match: type.name()='" + type->name() + "', but name='" + name + "'!");
	types[name] = std::move(type);
}

FunctionBaseWeakPtr Scope::addFunction(const cat::String& name, FunctionBasePtr&& function)
{
	NGPL_ASSERT2(function->name() == name, "Names don't match: function.name()='" + function->name() + "', but name='" + name + "'!");
	auto funcWeakPtr = function.weak();
	auto& overloads = functions[function->name()];
	cat::String reason;
	if (overloads.canAddOverload(function->signature(), reason)) {
		overloads.add(std::move(function));
	} else {
		NGPL_ASSERT2(false, reason);
	}
	return funcWeakPtr;
}

bool Scope::canAddFunction(const cat::String& name, const FunctionSignature& signature, cat::String& reasonOut) const
{
	auto functionsIt = functions.find(name);
	if (functionsIt != functions.end()) {
		//hasFoundName = true;
		return functionsIt->second.canAddOverload(signature, reasonOut);
	}
	return true;
}

VariableCWeakPtr Scope::tryGetVariable(const cat::String& name) const
{
	auto variablesIt = variables.find(name);
	if (variablesIt != variables.end()) {
		return variablesIt->second.weak();
	}
	return nullptr;
}

FunctionOverloadsCWeakPtr Scope::tryGetFunctionOverloads(const cat::String& name) const
{
	auto functionsIt = functions.find(name);
	if (functionsIt != functions.end()) {
		return &functionsIt->second;
	}
	return nullptr;
}

FunctionBaseCWeakPtr Scope::tryGetFunction(const cat::String& name, const CallArgs& args) const
{
	//bool hasFoundName = false;
	auto functionsIt = functions.find(name);
	if (functionsIt != functions.end()) {
		//hasFoundName = true;
		return functionsIt->second.tryGetOverload(args);
	}
	return nullptr;
}

TypeWeakPtr Scope::tryGetType(const cat::String& name)
{
	auto typesIt = types.find(name);
	if (typesIt != types.end()) {
		return typesIt->second.weak();
	}
	return nullptr;
}

TypeCWeakPtr Scope::tryGetType(const cat::String& name) const
{
	auto typesIt = types.find(name);
	if (typesIt != types.end()) {
		return typesIt->second.weak();
	}
	return nullptr;
}

cat::WriterObjectABC& Scope::print(cat::WriterObjectABC& s) const
{
	bool hasTypes = false;
	{
		auto typeIt = cat::range(types).map_c(LAMBDA(typePair)-> auto& { return typePair.second; }).iterate();
		if (not typeIt.isPastEnd()) {
			typeIt.get()->print(s);
			typeIt.advance();
			hasTypes = true;
		}
		while(not typeIt.isPastEnd()) {
			s += cat::nl;
			typeIt.get()->print(s);
			typeIt.advance();
		}
	}
	auto funcIt = cat::range(functions)
			.flatmap_c(LAMBDA(funcsPair) { return cat::range(funcsPair.second); })
			.map_c(LAMBDA(func)-> auto { return func.template as<Function>(); })
			.iterate();
	if (not hasTypes and not funcIt.isPastEnd()) {
		funcIt.get()->print(s);
		funcIt.advance();
	}
	while(not funcIt.isPastEnd()) {
		s += cat::nl;
		funcIt.get()->print(s);
		funcIt.advance();
	}

//	foreach_c(funcPair, cat::range(functions)
//			  .map_c(LAMBDA(funcsPair) { return cat::range(funcsPair.second); })
//			  .flatten()) {
//		s += cat::nl;
//		s += cat::nlIndent;
//		funcPair.second->print(s);
//	}

	//		cat::range(types)
	//				.map_c(LAMBDA(typePair) { cat::SW s; typePair.second->toString(s); return s.str(); })
	//				.addSeparator([&s](){ return "\n\n", s.getIndent(); } )
	//				.forEach_c(LAMBDA2(&s, str) { s += str; });

	//		cat::range(functions)
	//				.map_c(LAMBDA(funcsPair) { return cat::range(funcsPair.second); })
	//				.flatten()
	//				.map_c(LAMBDA(funcPair) { cat::SW s; funcPair.second->toString(s); return s.str(); })
	//				.addSeparator([](){ return "\n\n"; } )
	//				.forEach_c(LAMBDA2(&s, str) { s += str; });
	return s;
}


}

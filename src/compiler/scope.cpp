#include "scope.h"

namespace ngpl {

VariableCWeakPtr Scope::setVariable(const std::string& name, VariablePtr&& variable)
{
	NGPL_ASSERT2(not variable->isTemporary(), "Variables, added to a Scope must not be temporary! (variable " + name + ")")
	NGPL_ASSERT2(variables.find(name) == variables.end(), "Variable " + name + " is already in variables!")
	salt = std::max(salt, variable->address() + Address(variable->fixedSize()));
	return (variables[name] = std::move(variable)).getRaw();
}

VariableCWeakPtr Scope::addVariable(const std::string& name, VariablePtr&& variable)
{
	NGPL_ASSERT2(not variable->isTemporary(), "Variables, added to a Scope must not be temporary! (variable " + name + ")")
	NGPL_ASSERT2(variables.find(name) == variables.end(), "variable " + name + " is already in variables!")
	if (variable->referenceMode() != ReferenceMode::HEAP_VAL) {
		variable->_address = salt;
		salt += variable->fixedSize();
	}
	return (variables[name] = std::move(variable)).getRaw();
}

void Scope::addType(const std::string& name, TypeCPtr&& type)
{
	NGPL_ASSERT2(types.find(name) == types.end(), "type " + name + " is already in types!")
	(types[name] = std::move(type)).getRaw();
}

void Scope::addFunction(const std::string& name, FunctionSignature&& signature, FunctionCPtr&& function)
{
	auto& overloads = functions[name];
	NGPL_ASSERT2(overloads.find(signature) == overloads.end(), "function " + name + signature.asCodeString() + " is already in functions!")
	(overloads[std::move(signature)] = std::move(function)).getRaw();
}

VariableCWeakPtr Scope::tryGetVariable(const std::string& name) const
{
	auto variablesIt = variables.find(name);
	if (variablesIt != variables.end()) {
		return variablesIt->second.getRaw();
	}
	return nullptr;
}

FunctionBaseCWeakPtr Scope::tryGetFunction(const std::string& name, const FunctionSignature& signature) const
{
	//bool hasFoundName = false;
	auto functionsIt = functions.find(name);
	if (functionsIt != functions.end()) {
		//hasFoundName = true;
		auto overloadsIt = functionsIt->second.find(signature);
		if (overloadsIt != functionsIt->second.end()) {
			return overloadsIt->second.getRaw();
		}
	}
	return nullptr;
}

TypeCWeakPtr Scope::tryGetType(const std::string& name) const
{
	auto typesIt = types.find(name);
	if (typesIt != types.end()) {
		return typesIt->second.getRaw();
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
			s += cat::nlIndent;
			typeIt.get()->print(s);
			typeIt.advance();
		}
	}
	auto funcIt = cat::range(functions)
			.map_c(LAMBDA(funcsPair) { return cat::range(funcsPair.second); })
			.flatten()
			.map_c(LAMBDA(funcPair)-> auto& { return funcPair.second; })
			.iterate();
	if (not hasTypes and not funcIt.isPastEnd()) {
		funcIt.get()->print(s);
		funcIt.advance();
	}
	while(not funcIt.isPastEnd()) {
		s += cat::nl;
		s += cat::nlIndent;
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

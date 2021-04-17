#include "builtins.h"

#include "compiler/language/scope.h"

#include "ranges.h"

namespace ngpl {

#define GETVAL(index, type) args.pop().getValue<type>()

#define BI_OP(op_, rType, argsType, rcType, type, instrs) #op_, "", rType, {argsType, argsType}, false, [](CallStack& args){ auto rhs = GETVAL(0, type); auto lhs = GETVAL(1, type); return rcType(lhs op_ rhs); }, instrs
#define UN_OP(op_, rType, argType, rcType, type, instrs) #op_, "", rType, {argType}, false, [](CallStack& args){ auto rhs = GETVAL(0, type); return rcType(op_ rhs); }, instrs

struct BuiltinsScopeBuilder {
private:
	Scope scope{0};

	FunctionSignature makeSignature(cat::DynArray<cat::String> argTypes, const cat::String& retType) const
	{
		FunctionSignature signature {
			cat::range(argTypes)
				.map(LAMBDA2(&,t) { return TypeReference{scope.tryGetType(t)}; })
				.toContainer2<cat::DynArray>(),
			TypeReference{scope.tryGetType(retType)}
		};
		return signature;
	}

public:

	BuiltinsScopeBuilder& addType(uint64_t fixedSize, const cat::String& name)
	{
		 scope.addType(name, new Type{name, "", fixedSize, TypeKind::BASIC, true});
		 return *this;
	}

	BuiltinsScopeBuilder& addFunc(
		const cat::String& name,
		const cat::String& qualifier,
		const cat::String& retType,
		const cat::DynArray<cat::String>& argTypes,
		bool hasSideEffect,
		const BuiltinFunctionBody& body,
		const std::optional<std::vector<InstructionID>>& instructions
	)
	{
		auto signature = makeSignature(argTypes, retType);
		BuiltinFunctionPtr func{{}, name, qualifier, std::move(signature), hasSideEffect, body, instructions};
		scope.addFunction(std::move(func));
		return *this;
	}

	Scope build() { return std::move(scope);}

};

#define BI_OP(op_, rType, argsType, rcType, type, instrs) #op_, "", rType, {argsType, argsType}, false, [](CallStack& args){ auto rhs = GETVAL(0, type); auto lhs = GETVAL(1, type); return rcType(lhs op_ rhs); }, instrs
//#op_, "", rType, {argsType, argsType}, false, [](CallStack& args){ auto rhs = GETVAL(0, type); auto lhs = GETVAL(1, type); return rcType(lhs op_ rhs); }, instrs

Scope makeBuiltinsScope() {
	BuiltinsScopeBuilder builtins{};

	// Types:
	#define BUILTIN_TYPE_FACTORY(fixedSize, typeName, refereceMacroName) builtins.addType(fixedSize, typeName);
	#include "builtinTypes_inc.h"

	// Functions & Operators
	builtins.addFunc(BI_OP(+, "Int", "Int", int64_t, int64_t, {{InstructionID::ADD_SI}}));
	builtins.addFunc(BI_OP(+, "String", "String", cat::String, cat::String, std::nullopt));
	builtins.addFunc(UN_OP(+, "Int", "Int", int64_t, int64_t, {std::vector<InstructionID>()}));

	builtins.addFunc(BI_OP(-, "Int", "Int", int64_t, int64_t, {{InstructionID::SUB_SI}}));
	builtins.addFunc(UN_OP(-, "Int", "Int", int64_t, int64_t, {{InstructionID::NEG_SI}}));

	builtins.addFunc(BI_OP(*, "Int", "Int", int64_t, int64_t, {{InstructionID::MUL_SI}}));

	builtins.addFunc(BI_OP(/, "Int", "Int", int64_t, int64_t, {{InstructionID::DIV_SI}}));

	builtins.addFunc(BI_OP(%, "Int", "Int", int64_t, int64_t, {{InstructionID::REM_SI}}));

	builtins.addFunc(BI_OP(==, "Bool", "Int", int64_t, int64_t, std::nullopt));
	builtins.addFunc(BI_OP(==, "Bool", "String", int64_t, cat::String, std::nullopt));

	builtins.addFunc(BI_OP(!=, "Bool", "Int", int64_t, int64_t, {{InstructionID::XOR}}));
	builtins.addFunc(BI_OP(!=, "Bool", "String", int64_t, cat::String, std::nullopt));

	builtins.addFunc(BI_OP(<, "Bool", "Int", int64_t, int64_t, std::nullopt));

	builtins.addFunc(BI_OP(>, "Bool", "Int", int64_t, int64_t, std::nullopt));

	builtins.addFunc(BI_OP(<=, "Bool", "Int", int64_t, int64_t, std::nullopt));

	builtins.addFunc(BI_OP(>=, "Bool", "Int", int64_t, int64_t, std::nullopt));

	builtins.addFunc(BI_OP(or, "Bool", "Bool", int64_t, int64_t,  std::nullopt));

	builtins.addFunc(BI_OP(and, "Bool", "Bool", int64_t, int64_t,  std::nullopt));

	builtins.addFunc("print", "", "None", {"Bool"}, true,
		[](CallStack& args) {
			cat::OW out = cat::OW(std::cout);
			out += GETVAL(0, int64_t) ? "true" : "false";
			out += cat::nl;
			return None();
		}
	, std::nullopt);
	builtins.addFunc("print", "", "None", {"Int"}, true,
		[](CallStack& args) {
			cat::OW out = cat::OW(std::cout);
			out += GETVAL(0, int64_t);
			out += cat::nl;
			return None();
		}
	, std::nullopt);
	builtins.addFunc("print", "", "None", {"String"}, true,
		[](CallStack& args) {
			cat::OW out = cat::OW(std::cout);
			out += GETVAL(0, cat::String);
			out += cat::nl;
			return None();
		}
	, std::nullopt);

	builtins.addFunc("readln", "", "String", {}, true,
		[](CallStack& ) {
			cat::String line;
			std::getline( std::cin, line );
			return line;
		}
	, std::nullopt);

	builtins.addFunc( "Int", "", "Int", {"String"}, false,
		[](CallStack& args) {
			return std::stoll(GETVAL(0, cat::String));
		}
	, std::nullopt);

	builtins.addFunc( "String", "", "String", {"Int"}, false,
		[](CallStack& args) {
			return std::to_string   (GETVAL(0, int64_t));
		}
	, std::nullopt);

	builtins.addFunc( "sqr", "", "Int", {"Int"}, false,
		[](CallStack& args) {
			const auto v = GETVAL(0, int64_t);
			return v * v;
		}
	, std::nullopt);

	builtins.addFunc( "combine", "", "Int", {"Int", "Int", "Int"}, false,
		[](CallStack& args) {
		const auto v0 = GETVAL(0, int64_t);
		const auto v1 = GETVAL(0, int64_t);
		const auto v2 = GETVAL(0, int64_t);
			return v0 + v1 + v2;
		}
	, std::nullopt);

	return builtins.build();
}

const Scope builtins = makeBuiltinsScope();

TypeReference getBuiltinTypeRef(const cat::String& name, std::vector<TypeReference>&& arguments, bool isReference)
{
	auto type = builtins.tryGetType(name);
	NGPL_ASSERT(type != nullptr);
	return TypeReference(type, std::move(arguments), isReference);
}

TypeReference getBuiltinTypeRef(const cat::String& name, std::vector<TypeReference>&& arguments)
{
	bool isReference = (name).back() == '&';
	cat::String typeName;
	if (isReference) {
		typeName = name.substr(0, (name).size() - 2);
	} else {
		typeName = name;
	}
	return getBuiltinTypeRef(typeName, std::move(arguments), isReference);
}

}

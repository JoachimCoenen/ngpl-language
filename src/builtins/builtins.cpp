#include "builtins.h"

#include "compiler/language/scope.h"

#include "ranges.h"

namespace ngpl {

struct BuiltinsScopeBuilder {
public:
	using SimpleParameters = cat::DynArray<std::pair<cat::String, cat::String>>;
private:
	Scope scope{0_fa};

	FunctionSignature makeSignature(const SimpleParameters& params, const cat::String& retType) const
	{
		FunctionSignature signature {
			cat::range(params)
					.map(LAMBDA2(&, param) { return Parameter(param.first, getBuiltinTypeRef(scope, param.second, {})); })
				.toContainer2<cat::DynArray>(),
					getBuiltinTypeRef(scope, retType, {})
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
		const SimpleParameters& params,
		bool hasSideEffect,
		const BuiltinFunctionBody& body,
		const std::optional<std::vector<InstructionID>>& instructions
	)
	{
		auto signature = makeSignature(params, retType);
		BuiltinFunctionPtr func{{}, name, qualifier, std::move(signature), hasSideEffect, body, instructions};
		scope.addFunction(name, std::move(func));
		return *this;
	}

	Scope build() { return std::move(scope);}

};


#define GETVAL(index, type) args.pop().getValue<type>()
// #define BI_OP(op_, rType, argsType, rcType, type, instrs) #op_, "", rType, {argsType, argsType}, false, [](CallStack& args){ auto rhs = GETVAL(0, type); auto lhs = GETVAL(1, type); return rcType(lhs op_ rhs); }, instrs
#define UN_OP(op_, rType, argType, rcType, type, instrs) #op_, "", rType, {{"operand", argType}}, false, [](CallStack& args){ auto rhs = GETVAL(0, type); return rcType(op_ rhs); }, instrs
#define BI_OP(op_, rType, argsType, rcType, type, instrs) #op_, "", rType, {{"lhs", argsType}, {"rhs", argsType}}, false, [](CallStack& args){ auto rhs = GETVAL(0, type); auto lhs = GETVAL(1, type); return rcType(lhs op_ rhs); }, instrs
#define BI_OP2(op_, rType, arg1Type, arg2Type, rcType, type1, type2, instrs) #op_, "", rType, {{"lhs", arg1Type}, {"rhs", arg2Type}}, false, [](CallStack& args){ auto rhs = GETVAL(0, type2); auto lhs = GETVAL(1, type1); return rcType(lhs op_ rhs); }, instrs

void dodo(Reference& a, int64_t& b) {
	a+b;
}

Scope makeBuiltinsScope() {
	BuiltinsScopeBuilder builtins{};

	// Types:
	#define BUILTIN_TYPE_FACTORY(fixedSize, typeName, refereceMacroName) builtins.addType(fixedSize, typeName);
	#include "builtinTypes_inc.h"

	// Functions & Operators
	builtins.addFunc(BI_OP(+, "Int", "Int", int64_t, int64_t, {{InstructionID::ADD_SI}}));
	builtins.addFunc(BI_OP(+, "String", "String", cat::String, cat::String, std::nullopt));
	builtins.addFunc(BI_OP2(+, "&Any", "&Any", "Int", Reference, Reference, int64_t,  {{InstructionID::ADD_R}}));
	builtins.addFunc(UN_OP(+, "Int", "Int", int64_t, int64_t, {std::vector<InstructionID>()}));

	builtins.addFunc(BI_OP(-, "Int", "Int", int64_t, int64_t, {{InstructionID::SUB_SI}}));
	builtins.addFunc(UN_OP(-, "Int", "Int", int64_t, int64_t, {{InstructionID::NEG_SI}}));

	builtins.addFunc(BI_OP(*, "Int", "Int", int64_t, int64_t, {{InstructionID::MUL_SI}}));

	builtins.addFunc(BI_OP(/, "Int", "Int", int64_t, int64_t, {{InstructionID::DIV_SI}}));

	builtins.addFunc(BI_OP(%, "Int", "Int", int64_t, int64_t, {{InstructionID::REM_SI}}));

	builtins.addFunc(BI_OP(==, "Bool", "Int", int64_t, int64_t, std::nullopt));
	builtins.addFunc(BI_OP(==, "Bool", "String", int64_t, cat::String, std::nullopt));
	builtins.addFunc(BI_OP(==, "Bool", "&Any", int64_t, Reference, std::nullopt));

	builtins.addFunc(BI_OP(!=, "Bool", "Int", int64_t, int64_t, {{InstructionID::XOR}}));
	builtins.addFunc(BI_OP(!=, "Bool", "String", int64_t, cat::String, std::nullopt));

	builtins.addFunc(BI_OP(<, "Bool", "Int", int64_t, int64_t, std::nullopt));

	builtins.addFunc(BI_OP(>, "Bool", "Int", int64_t, int64_t, std::nullopt));

	builtins.addFunc(BI_OP(<=, "Bool", "Int", int64_t, int64_t, std::nullopt));

	builtins.addFunc(BI_OP(>=, "Bool", "Int", int64_t, int64_t, std::nullopt));

	builtins.addFunc(BI_OP(or, "Bool", "Bool", int64_t, int64_t,  std::nullopt));
	builtins.addFunc(BI_OP(and, "Bool", "Bool", int64_t, int64_t,  std::nullopt));

	builtins.addFunc("memalloc", "", "&Any", {{"size", "Int"}}, true,
		[](CallStack& args) {
			GETVAL(0, int64_t);
			return None();
		}
	, {{InstructionID::HEAP_ALLOC}});
	builtins.addFunc("memfree", "", "None", {{"size", "&Any"}}, true,
		[](CallStack& args) {
			GETVAL(0, Reference);
			return None();
		}
	, {{InstructionID::HEAP_DEALLOC}});

	builtins.addFunc("print", "", "None", {{"v", "Bool"}}, true,
		[](CallStack& args) {
			cat::OW out = cat::OW(std::cout);
			out += GETVAL(0, int64_t) ? "true" : "false";
			out += cat::nl;
			return None();
		}
	, std::nullopt);
	builtins.addFunc("print", "", "None", {{"v", "Int"}}, true,
		[](CallStack& args) {
			cat::OW out = cat::OW(std::cout);
			out += GETVAL(0, int64_t);
			out += cat::nl;
			return None();
		}
	, std::nullopt);
	builtins.addFunc("print", "", "None", {{"v", "String"}}, true,
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

	builtins.addFunc( "Int", "", "Int", {{"v", "String"}}, false,
		[](CallStack& args) {
			return std::stoll(GETVAL(0, cat::String));
		}
	, std::nullopt);

	builtins.addFunc( "String", "", "String", {{"v", "Int"}}, false,
		[](CallStack& args) {
			return std::to_string   (GETVAL(0, int64_t));
		}
	, std::nullopt);

	builtins.addFunc( "sqr", "", "Int", {{"v", "Int"}}, false,
		[](CallStack& args) {
			const auto v = GETVAL(0, int64_t);
			return v * v;
		}
	, std::nullopt);

	builtins.addFunc( "combine", "", "Int", {{"v1", "Int"}, {"v2", "Int"}, {"v3", "Int"}}, false,
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

TypeReference getBuiltinTypeRef(const ngpl::Scope& scope, const cat::String& name, std::vector<TypeReference>&& arguments, int pointerDepth, bool isReference)
{
	auto type = scope.tryGetType(name);
	NGPL_ASSERT(type != nullptr);
	return TypeReference(type, std::move(arguments), pointerDepth, isReference);
}

TypeReference getBuiltinTypeRef(const ngpl::Scope& scope, const cat::String& name, std::vector<TypeReference>&& arguments)
{
	bool isReference = (name).back() == '%';

	size_t i = 0;

	while (i < name.size() and name[i] == '&') {
		++i;
	}
	const auto pointerDepth = i;
	cat::String typeName;
	if (isReference) {
		typeName = name.substr(i, name.size() - i - 1);
	} else {
		typeName = name.substr(i);
	}
	return getBuiltinTypeRef(scope, typeName, std::move(arguments), pointerDepth, isReference);
}

}

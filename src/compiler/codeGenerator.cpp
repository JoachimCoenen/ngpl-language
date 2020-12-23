#include "codeGenerator.h"

#include "ranges.h"


namespace ngpl {

//    Instruction
using Instrs = intermediate::Instructions;

#define ADD_TYPE(builtinTypes, fixedSize, name) builtinTypes.insert_or_assign(name, new Type{name, "", fixedSize, true, true})
#define ADD_TYPE2(name) {#name, {#name}}


std::unordered_map<std::string, TypeCPtr> makeBuiltinTypesMap() {
	std::unordered_map<std::string, TypeCPtr> builtinTypes;

	ADD_TYPE(builtinTypes, 0, "None");
	ADD_TYPE(builtinTypes, 1, "Bool");

	ADD_TYPE(builtinTypes, 1,  "Int8");
	ADD_TYPE(builtinTypes, 1, "UInt8");
	ADD_TYPE(builtinTypes, 1,  "Int16");
	ADD_TYPE(builtinTypes, 1, "UInt16");
	ADD_TYPE(builtinTypes, 1,  "Int");
	ADD_TYPE(builtinTypes, 1, "UInt");
	ADD_TYPE(builtinTypes, 1,  "Int64");
	ADD_TYPE(builtinTypes, 1, "UInt64");

	ADD_TYPE(builtinTypes, 1, "Float");
	ADD_TYPE(builtinTypes, 1, "Float64");
	ADD_TYPE(builtinTypes, 1, "Float128");

	ADD_TYPE(builtinTypes, 1, "String");
	/*
	 * Ord Bool Byte Word Char
	 * Int UInt Int8 UInt8
	 * Int16 UInt16 Int32 UInt32
	 * Int64 UInt64
	 * Float Float64 Float128
	 * String Array Set Dictionary List
	 */

	return builtinTypes;
}

const std::unordered_map<std::string, TypeCPtr> CodeGenerator::builtinTypes = makeBuiltinTypesMap();


#define GETVAL(index, type) args.pop().getValue<type>()

#define GET_BUILIN_TYPE(name) CodeGenerator::builtinTypes.at(name).getRaw()

std::pair<std::string, std::unordered_map<FunctionSignature, BuiltinFunction>> builtinFuncOverloads(
		std::string&& name,
		std::unordered_map<FunctionSignature, BuiltinFunction>&& functions)
{
	return {std::move(name), std::move(functions)};
}

std::pair<FunctionSignature, BuiltinFunction> builtinFunc(
		const std::string& name,
		const std::string& qualifier,
		const std::string& resType,
		std::vector<std::string> argTypes,
		bool hasSideEffect,
		const std::function<Value(cat::Stack<Value>&)>& body,
		const std::optional<std::vector<InstructionID>>& instructions
		)
{
	FunctionSignature signature = {
		cat::range(argTypes)
		.map(LAMBDA(t) { return GET_BUILIN_TYPE(t); })
		.toVector()
	};
	return {signature, BuiltinFunction{name, qualifier, signature, GET_BUILIN_TYPE(resType), hasSideEffect, body, instructions}};
}

#define BI_OP(op_, rType, argsType, rcType, type, instrs) builtinFunc(#op_, "", rType, {argsType, argsType}, false, [](cat::Stack<Value>& args){ auto rhs = GETVAL(0, type); auto lhs = GETVAL(1, type); return rcType(lhs op_ rhs); }, instrs)


#define UN_OP(op_, rType, argType, rcType, type, instrs) builtinFunc(#op_, "", rType, {argType}, false, [](cat::Stack<Value>& args){ auto rhs = GETVAL(0, type); return rcType(op_ rhs); }, instrs)

const std::unordered_map<std::string, std::unordered_map<FunctionSignature, BuiltinFunction>> CodeGenerator::builtinFunctions {
	{ "+", {
			BI_OP(+, "Int", "Int", int64_t, int64_t, {{InstructionID::ADD_SI}}),
			BI_OP(+, "String", "String", std::string, std::string, std::nullopt),
			UN_OP(+, "Int", "Int", int64_t, int64_t, {std::vector<InstructionID>()}),
		}
	}, {  "-", {
		BI_OP(-, "Int", "Int", int64_t, int64_t, {{InstructionID::SUB_SI}}),
		UN_OP(-, "Int", "Int", int64_t, int64_t, {{InstructionID::NEG_SI}}),
		}
	}, { "*", {
		BI_OP(*, "Int", "Int", int64_t, int64_t, {{InstructionID::MUL_SI}}),
		}
	}, { "/", {
		BI_OP(/, "Int", "Int", int64_t, int64_t, {{InstructionID::DIV_SI}}),
		}
	}, { "%", {
		BI_OP(%, "Int", "Int", int64_t, int64_t, {{InstructionID::REM_SI}}),
		}
	}, { "==", {
		BI_OP(==, "Bool", "Int", int64_t, int64_t, std::nullopt),
		BI_OP(==, "Bool", "String", int64_t, std::string, std::nullopt),
		}
	}, { "!=", {
		BI_OP(!=, "Bool", "Int", int64_t, int64_t, {{InstructionID::XOR}}),
		BI_OP(!=, "Bool", "String", int64_t, std::string, std::nullopt),
		}
	}, { "<", {
		BI_OP(<, "Bool", "Int", int64_t, int64_t, std::nullopt),
		}
	}, { ">", {
		BI_OP(>, "Bool", "Int", int64_t, int64_t, std::nullopt),
		}
	}, { "<=", {
		BI_OP(<=, "Bool", "Int", int64_t, int64_t, std::nullopt),
		}
	}, { ">=", {
		BI_OP(>=, "Bool", "Int", int64_t, int64_t, std::nullopt),
		}
	}, { "or", {
		BI_OP(or, "Bool", "Bool", int64_t, int64_t,  std::nullopt),
		}
	}, { "and", {
		BI_OP(and, "Bool", "Bool", int64_t, int64_t,  std::nullopt),
		}
	},


	{ "print", {
		builtinFunc("print", "", "None", {"Bool"}, true,
			[](cat::Stack<Value>& args) {
				cat::OW out = cat::OW(std::cout);
				out += GETVAL(0, int64_t) ? "true" : "false";
				out += cat::nl;
				return None();
			}
		, std::nullopt),
		builtinFunc("print", "", "None", {"Int"}, true,
			[](cat::Stack<Value>& args) {
				cat::OW out = cat::OW(std::cout);
				out += GETVAL(0, int64_t);
				out += cat::nl;
				return None();
			}
		, std::nullopt),
		builtinFunc("print", "", "None", {"String"}, true,
			[](cat::Stack<Value>& args) {
				cat::OW out = cat::OW(std::cout);
				out += GETVAL(0, std::string);
				out += cat::nl;
				return None();
			}
		, std::nullopt),
		}
	}, { "readln", {
		builtinFunc("readln", "", "String", {}, true,
			[](cat::Stack<Value>& ) {
				std::string line;
				std::getline( std::cin, line );
				return line;
			}
		, std::nullopt),
		}
	}, { "Int", {
		builtinFunc( "Int", "", "Int", {"String"}, false,
			[](cat::Stack<Value>& args) {
				return std::stoll(GETVAL(0, std::string));
			}
		, std::nullopt),
		}
	}, { "String", {
		builtinFunc( "String", "", "String", {"Int"}, false,
			[](cat::Stack<Value>& args) {
				return std::to_string   (GETVAL(0, int64_t));
			}
		, std::nullopt),
		}
	},

};

}

namespace ngpl {

CodeGenerator::CodeGenerator()
{
	//pushScope(true);
	// add the stackFramePtr:
	// currentScope()->addVariable("*stackFramePtr", getType("Int", Position()));
}

void CodeGenerator::evalRoot(const RootCWeakPtr& root)
{
	//writeBuiltinFunctions();
	//addInstruction(Instrs::PushInt(uint64_t(0), root->pos));
	//addInstruction(Instrs::WriteFA(uint64_t(0), root->pos));

	foreach_c(stmt, root->statements) {
		evalStatement(stmt.getRaw());
	}

	//addInstruction(Instrs::Nop(Position()));
}

UnitPtr CodeGenerator::evalUnitDeclaration(const UnitDeclarationCWeakPtr& unitDecl)
{
	const auto& name = unitDecl->name;
	const auto& unitNature = unitDecl->unitNature;
	UnitPtr unit = new Unit(name, nullptr, unitNature);
	codeContainerStack.push_back(&unit->body());
	pushScope(true);

	foreach_c(innerDecl, unitDecl->block->statements) {
		evalStatement(innerDecl.getRaw());
	}

	unit->scope() = new Scope(std::move(*currentScope()));
	popScope();

	return unit;
}


TypeCWeakPtr CodeGenerator::evalLiteral(const LiteralCWeakPtr& literal)
{
	if (auto literalBool = literal.as<LiteralBool>()) {
		addInstruction(Instrs::PushInt(uint64_t(literalBool->get()), literalBool->pos));
		return getType("Bool", literal->pos);
	}
	if (auto literalInt = literal.as<LiteralInt>()) {
		addInstruction(Instrs::PushInt(literalInt->get(), literalInt->pos));
		return getType("Int", literal->pos);
	}
	if (auto literalString = literal.as<LiteralString>()) {
		addInstruction(Instrs::PushStr(literalString->get(), literalString->pos));
		return getType("String", literal->pos);
	}

	throw SyntaxError(cat::SW() << "Unknow Literal type! (compiler is broken!)'" << literal->getTypeName() << "'.", literal->pos);
}

TypeCWeakPtr CodeGenerator::evalExpression(const ExpressionCWeakPtr& expr)
{
	if (auto literal = expr.as<Literal>()) {
		return evalLiteral(literal);
	}

	if (auto variableRef = expr.as<VariableReference>()) {
		auto variable = evalVariableReference(variableRef);
		readFromVariable(&variable, variableRef->pos);
		return variable.type();
	}

	if (auto funcCall = expr.as<FunctionCall>()) {
		const auto& name = funcCall->name;

		TypeCWeakPtr parentType = nullptr;
		const bool isMethod = funcCall->parent != nullptr;
		if (isMethod) {
			parentType = evalExpression(funcCall->parent.getRaw());
			// next line is commented out, bc. 'self' variable is temporarely passed as a STACK_VAL and not a STACK_REF:
			// addInstruction(Instrs::PushInt( /*cat::range(args).map(LAMBDA(v){ return v->fixedSize(); }).join() +*/ parentType->fixedSize(), funcCall->pos ));
		}

		std::vector<TypeCWeakPtr> argsTmp = cat::range(funcCall->arguments)
			.map(LAMBDA2(this, v) {return this->evalExpression(v.getRaw()); })
			.toVector();
		std::vector<TypeCWeakPtr> args;
		args.insert(args.end(), argsTmp.rbegin(),argsTmp.rend());

		FunctionBaseCWeakPtr func;
		if (isMethod) {
			func = parentType->scope()->tryGetFunction(name, FunctionSignature{std::move(args)});
			if (not func) {
				throw SyntaxError(cat::SW() << "unknown member '" << funcCall->name << "' in object of type " << parentType->name() << ".", funcCall->pos);
			}
		} else {
			func = getFunction(name, FunctionSignature{std::move(args)}, funcCall->pos);
		}
		evalFunction(func, funcCall->pos);


		if (isMethod) {
			// cleaning up...
			// current stack Layout:
			// ...|parent|returnValue|
			// desired stack Layout:
			// ...|returnValue|
			auto baseAddr = currentScope()->getFrameSize() + tempsOnStack- func->returnType()->fixedSize();

			if (auto selfRef = funcCall->parent.as<VariableReference>()) {

				readFromStackFrameF(baseAddr - parentType->fixedSize(), parentType->fixedSize(), funcCall->pos);
				auto selfVar = evalVariableReference(selfRef);
				writeToVariable(&selfVar, funcCall->pos);
			}

			readFromStackFrameF(baseAddr, func->returnType()->fixedSize(), funcCall->pos);
			writeToStackFrameF(baseAddr - parentType->fixedSize(), func->returnType()->fixedSize(), funcCall->pos);
			for (auto i = parentType->fixedSize(); i --> 0; ) {
				addInstruction(Instrs::PopVal(funcCall->pos));
			}
		}


		return func->returnType();
	}

	if (auto operCall = expr.as<BinaryOperatorCall>()) {
		const auto& name = operCall->name;
		std::vector<TypeCWeakPtr> args = {
			/*lhs*/ this->evalExpression(operCall->lhs.getRaw()),
			/*rhs*/ this->evalExpression(operCall->rhs.getRaw())
		};
		args = {args.rbegin(), args.rend()};
		const auto& func = getFunction(name, FunctionSignature{std::move(args)}, operCall->pos);
		evalFunction(func, operCall->pos);

		return func->returnType();
	}
	if (auto operCall = expr.as<UnaryOperatorCall>()) {
		const auto& name = operCall->name;
		std::vector<TypeCWeakPtr> args = {
			this->evalExpression(operCall->operand.getRaw()),
		};
		const auto& func = getFunction(name, FunctionSignature{std::move(args)}, operCall->pos);
		evalFunction(func, operCall->pos);

		return func->returnType();
	}

	throw SyntaxError(cat::SW() << "Unknow ExpressinType '" << expr->getTypeName() << "'.", expr->pos);
}

TypeCWeakPtr CodeGenerator::evalFunction(const FunctionBaseCWeakPtr& func, const Position& pos)
{
	if (auto fn = func.as<Function>()) {
		// handle stack frame:
//		addInstruction(Instrs::PushInt(currentScope().salt, pos));
//		addInstruction(Instrs::ReadFA(0, pos));

//		const auto& add = tryGetBuiltinFunction("+", FunctionSignature{{ GET_BUILIN_TYPE("Int"), GET_BUILIN_TYPE("Int") }});
//		addInstruction(Instrs::Call(&(add->_body), pos));

//		addInstruction(Instrs::WriteFA(0, pos));

		// call function:
		addInstruction(Instrs::Call(fn.___getPtr(), pos));


		// handle stack frame:
//		addInstruction(Instrs::PushInt(currentScope().salt, pos));
//		addInstruction(Instrs::ReadFA(0, pos));

//		const auto& sub = tryGetBuiltinFunction("-", FunctionSignature{{ GET_BUILIN_TYPE("Int"), GET_BUILIN_TYPE("Int") }});
//		addInstruction(Instrs::Call(&sub->_body, pos));

//		addInstruction(Instrs::WriteFA(0, pos));

	} else if (auto fn = func.as<BuiltinFunction>()) {
		if (fn->_instructions.has_value()) {
			foreach_c(instrId, fn->_instructions.value()) {
				addInstruction(InterInstr(instrId, None_{}, pos));
			}
			return func->returnType();
		} else {
			addInstruction(Instrs::Call(fn.___getPtr(), pos));
		}
	}
	tempsOnStack += func->stackDelta();

	return func->returnType();

}

void CodeGenerator::evalStatement(const StatementCWeakPtr& stmt)
{
	NGPL_ASSERT(tempsOnStack == 0);

	if (auto expr = stmt.as<Expression>()) {
		auto type = evalExpression(expr);
		for (auto i = type->fixedSize(); i --> 0; ) {
			addInstruction(Instrs::PopVal(expr->pos));
		}
	}

	else if (auto assignment = stmt.as<Assignment>()) {
		auto variable = evalVariableReference(assignment->variable.getRaw());
		auto type = evalExpression(assignment->expr.getRaw());
		checkType(variable.type(), type, assignment->expr->pos);

		writeToVariable(&variable, assignment->pos);
	}

	else if (auto ifControl = stmt.as<IfControl>()) {
		const auto conditionType = evalExpression(ifControl->condition.getRaw());
		checkType(GET_BUILIN_TYPE("Bool"), conditionType, ifControl->condition->pos);
		tempsOnStack -= 1;

		itm::IntermediateCodeContainer ifCode;
		codeContainerStack.push_back(&ifCode);
		evalBlock(ifControl->thenBlock.getRaw());
		codeContainerStack.pop_back();

		itm::IntermediateCodeContainer elseCode;
		if (ifControl->elseBlock) {
			codeContainerStack.push_back(&elseCode);
			evalBlock(ifControl->elseBlock.getRaw());
			codeContainerStack.pop_back();
		}

		addInstruction(new itm::IntermediateIf(false, std::move(ifCode), std::move(elseCode), ifControl->pos));

	}

	else if (auto whileControl = stmt.as<WhileControl>()) {
		//const auto whileConditionPos = getCurrentPos();
		itm::IntermediateCodeContainer code;
		codeContainerStack.push_back(&code);

		const auto conditionType = evalExpression(whileControl->condition.getRaw());
		checkType(GET_BUILIN_TYPE("Bool"), conditionType, whileControl->condition->pos);
		tempsOnStack -= 1;

		itm::IntermediateCodeContainer ifCode;
		codeContainerStack.push_back(&ifCode);
		addInstruction(new itm::IntermediateSpecial(itm::IntermediateSpecialId::BREAK, whileControl->pos));
		codeContainerStack.pop_back();

		itm::IntermediateCodeContainer elseCode;
		addInstruction(new itm::IntermediateIf(true, std::move(ifCode), std::move(elseCode), whileControl->pos));



		evalBlock(whileControl->block.getRaw());
		addInstruction(new itm::IntermediateSpecial(itm::IntermediateSpecialId::CONTINUE, whileControl->pos));
		codeContainerStack.pop_back();
		addInstruction(new itm::IntermediateLoop(std::move(code), whileControl->pos));
	}

	else if (auto returnStmt = stmt.as<ReturnStatement>()) {
		auto type = evalExpression(returnStmt->expr.getRaw());
		auto variable = getVariable("*return", returnStmt->pos);
		checkType(variable->type(), type, returnStmt->expr->pos);

		// "save" result:
		const auto returnSize = variable->address() + variable->fixedSize();
		writeToVariable(variable, returnStmt->pos);
		// cleanup stack:
		cleanupStack(currentScope()->getFrameSize() - returnSize, returnStmt->pos);
//		NGPL_ASSERT(tempsOnStack == 0);
//		for (auto i = currentScope().salt; i --> returnSize;) {
//			addInstruction(Instrs::PopVal(returnStmt->pos));
//		}
//		tempsOnStack = 0;

		addInstruction(new itm::IntermediateSpecial(itm::IntermediateSpecialId::RETURN, returnStmt->pos));
	}

	else if (auto decl = stmt.as<Declaration>()) {
		evalDeclaration(decl);
	}

	else {
		throw SyntaxError(cat::SW() << "Unknow StatementType '" << stmt->getTypeName() << "'.", stmt->pos);
	}

	NGPL_ASSERT(tempsOnStack == 0);

}

void CodeGenerator::evalBlock(const BlockCWeakPtr& block)
{
	pushScope();
	foreach_c(stmt, block->statements) {
		evalStatement(stmt.getRaw());
	}

	auto oldScope = std::move(*currentScope());
	popScope();
	// cleanup stack:
	cleanupStack(oldScope.getFrameSize() - currentScope()->getFrameSize(), block->pos);
//	for (auto i = oldScope.salt; i --> currentScope().salt;) {
//		addInstruction(Instrs::PopVal(block->pos));
//	}
}

void CodeGenerator::evalDeclaration(const DeclarationCWeakPtr& decl)
{
	NGPL_ASSERT(tempsOnStack == 0);
	if (auto varDecl = decl.as<VarDeclaration>()) {
		const auto& name = decl->name;
		if (hasVariable(name)) {
			throw SyntaxError(cat::SW() << "redefinition of '" << name << "'.", decl->pos);
		}

		TypeCWeakPtr type = nullptr;
		TypeCWeakPtr declType = nullptr;
		if (varDecl->initExpr) {
			NGPL_ASSERT(tempsOnStack == 0);
			type = evalExpression(varDecl->initExpr.getRaw());
			NGPL_ASSERT(tempsOnStack == int64_t(type->fixedSize()));
		}
		if (varDecl->type) {
			declType = getType(varDecl->type->name, varDecl->type->pos);
		}
		if (type and declType) {
			checkType(declType, type, varDecl->initExpr->pos);
		}
		if (declType) {
			type = declType;
		}
		if (not type->isFinished()) {
			throw SyntaxError(cat::SW() << "Illegal use of unfinished type '" << type->name() << "'.", varDecl->pos);
		}

		const bool isConst = decl.as<ConstDeclaration>();
		const bool isGlobal = this->isGlobal();
		if (isGlobal) {
			auto variable = currentScope()->addVariable(name, new Variable(type, staticHeapSize, ReferenceMode::HEAP_VAL, true, isConst));
			staticHeapSize += variable->type()->fixedSize();
			writeToHeapF(variable->address(), type->fixedSize(), varDecl->pos);
		} else {
			currentScope()->addVariable(name, new Variable(type, 0, ReferenceMode::STACK_VAL, true, isConst));
			NGPL_ASSERT(tempsOnStack == int64_t(type->fixedSize()) or tempsOnStack == 0);
			tempsOnStack = 0;
		}

		if (not varDecl->initExpr) {
			NGPL_ASSERT(tempsOnStack == 0);
			for (auto i = type->fixedSize(); i --> 0;) {
				addInstruction(Instrs::PushInt(0, decl->pos));
			}
			tempsOnStack = 0;
		}
//		if (varDecl->initExpr) {
//			writeToStackFrame(relAddr, type->fixedSize(), decl->pos);
//		}
	}

	else if (auto funcDecl = decl.as<FuncDeclaration>()) {
		//const auto jmpOverFuncDeclPos = addInstruction(Instrs::Nop(funcDecl->pos));
		//const auto funcEntryPos = getCurrentPos();

		pushScope(true);

		TypeWeakPtr selfType = typeStack.empty() ? nullptr : typeStack.peek();
		bool isMethod = selfType != nullptr;


		// handle self reference:
		if (isMethod) {
			// 'self' variable is temporarely passed as a STACK_VAL and not a STACK_REF:
			currentScope()->addVariable("self", new Variable(selfType, 0, ReferenceMode::STACK_VAL, true, false));
		}
		// handle function arguments / parameters:
		std::vector<TypeCWeakPtr> argumentTypes;
		foreach_c(param, funcDecl->parameters) {
			const auto& name = param->name;
			if (hasVariable(name)) {
				throw SyntaxError(cat::SW() << "redefinition of '" << name << "'.", param->pos);
			}

			auto type = getType(param->type->name, param->type->pos);
			argumentTypes.push_back(type);

			//auto relAddr = currentScope()->addVariable(name, type);
			currentScope()->addVariable(name, new Variable(type, 0, ReferenceMode::STACK_VAL, true, false));
			// writeToStackFrame(relAddr, type->fixedSize(), param->pos);
		}
		FunctionSignature signature{std::move(argumentTypes)};


		TypeCWeakPtr returnType = nullptr;
		if (funcDecl->returnType) {
			returnType = getType(funcDecl->returnType->name, funcDecl->returnType->pos);
		}
		else {
			returnType = GET_BUILIN_TYPE("None");
		}

		const auto qualifier = isMethod ? selfType->asQualifiedCodeString() + "." : "";
		FunctionWeakPtr function = new Function(funcDecl->name, qualifier, signature, returnType, selfType, true);
		if (hasFunction(funcDecl->name, signature)) {
			throw SyntaxError(cat::SW() << "redefinition of '" << funcDecl->name << "(" << signature.asCodeString() << ")'.", decl->pos);
		}
		(*(scopeStack.end()-2))->addFunction(funcDecl->name, std::move(signature), function.___getPtr());
		codeContainerStack.push_back(&function->body());

		//currentScope()->addVariable("*return", returnType);
		VariableCWeakPtr returnVar;
		const auto oldFrameSize = currentScope()->getFrameSize();
		if (isMethod) {
			// next lines added, bc. 'self' variable is temporarely passed as a STACK_VAL and not a STACK_REF:
			Address retValAddr = selfType->fixedSize();
			returnVar = currentScope()->setVariable("*return", new Variable(returnType, retValAddr, ReferenceMode::STACK_VAL, false, false));
		} else {
			returnVar = currentScope()->setVariable("*return", new Variable(returnType, 0, ReferenceMode::STACK_VAL, false, false));
		}

		// make sure there's enough room for the return value:
		const auto newFrameSize = currentScope()->getFrameSize();
		if (newFrameSize > oldFrameSize) {
			NGPL_ASSERT(tempsOnStack == 0);
			addInstruction(Instrs::PushInt(0, decl->pos));
			for (auto i = oldFrameSize+1; i < newFrameSize; ++i ) {
				addInstruction(Instrs::Dup(0, decl->pos));
			}
			tempsOnStack = 0;
		}

		// handle function Body:
		evalBlock(funcDecl->block.getRaw());

		auto oldScope = currentScope();
		// cleanup stack:
		cleanupStack(oldScope->getFrameSize() - (returnVar->address() + returnVar->fixedSize()), funcDecl->pos);
//		for (auto i = oldScope.salt; i --> returnType->fixedSize();) {
//			addInstruction(Instrs::PopVal(funcDecl->pos));
//		}
		// return
		addInstruction(new itm::IntermediateSpecial(itm::IntermediateSpecialId::RETURN, funcDecl->pos));

		//setInstruction(jmpOverFuncDeclPos, Instrs::JumpFA(getCurrentPos(), funcDecl->pos));
		popScope();
		codeContainerStack.pop_back();
	}

	else if (auto typeDecl = decl.as<StructDeclaration>()) {
		//const auto jmpOverFuncDeclPos = addInstruction(Instrs::Nop(typeDecl->pos));

		const auto& name = decl->name;
		if (hasVariable(name) or hasType(name)) {
			throw SyntaxError(cat::SW() << "redefinition of '" << name << "'.", decl->pos);
		}

		TypeWeakPtr parentType = typeStack.empty() ? nullptr : typeStack.peek();
		const auto qualifier = parentType != nullptr ? parentType->asQualifiedCodeString() + "." : "";
		TypeWeakPtr type = new Type(name, qualifier, 0 , false);
		currentScope()->addType(name, TypePtr(type.___getPtr()));
		codeContainerStack.push_back(&type->body());
		typeStack.push(type);
		pushScope(true);

		foreach_c(innerDecl, typeDecl->members) {
			if (auto varDecl = innerDecl.as<VarDeclaration>()){
				evalDeclaration(innerDecl.getRaw());
			}
		}

		// copy pointer over:
		type->scope().___getPtr() = currentScope().___getPtr();

		// finalize the type:
		type->finish();
		type->fixedSize() = currentScope()->getFrameSize();
		foreach_c(innerDecl, typeDecl->members) {
			if (not innerDecl.as<VarDeclaration>()) {
				evalDeclaration(innerDecl.getRaw());
			}
		}

		// make sure only ONE owns the pointer:
		scopeStack.back().___getPtr() = nullptr;
		//type->scope() = new Scope(std::move(currentScope()));
		popScope();
		typeStack.pop();
		codeContainerStack.pop_back();

		//setInstruction(jmpOverFuncDeclPos, Instrs::JumpFA(getCurrentPos(), typeDecl->pos));
	}

	NGPL_ASSERT(tempsOnStack == 0);
}

//void CodeGenerator::writeBuiltinFunctions()
//{
//	const auto jmpOverBuiltinsPos = addInstruction(Instrs::Nop(Position(0, 0, 0)));

//	for(const auto& [name, overloads] : builtinFunctions) {
//		for(const auto& [signature, overload] : overloads) {
//		const auto funcEntryPos = addInstruction(Instrs::Call(&overload._body, Position(0, 0, 0)));
//		addInstruction(Instrs::PopCntr(Position(0,0,0)));
//		addInstruction(Instrs::JumpDA(Position(0,0,0)));

//		currentScope().functions[name].insert({signature, new Function(name, signature, overload.returnType(), funcEntryPos)});
//		}
//	}
//	setInstruction(jmpOverBuiltinsPos, Instrs::JumpFA(getCurrentPos(), Position(0, 0, 0)));
//}

cat::WriterObjectABC& CodeGenerator::toString(cat::WriterObjectABC& s) const
{
//	cat::range(_instructions)
//			.map_c(LAMBDA(instr) { return instr.toString(); })
//			.addSeparator([](){ return "\n"; } )
//			.forEach_c(LAMBDA2(&s, str) { s += str; });
	return s;
}

}

namespace ngpl {

VariableCWeakPtr CodeGenerator::tryGetVariable(const std::string& name) const
{
	foreach_c(scope, cat::reversed(scopeStack)) {
		if (auto variable = scope->tryGetVariable(name)) {
			return variable;
		}
	}
	return nullptr;
}

VariableCWeakPtr CodeGenerator::getVariable(const std::string& name, const Position& pos) const
{
	if (auto variable = tryGetVariable(name)) {
		return variable;
	} else {
		throw SyntaxError(cat::SW() << "unknown variable '" << name << "'.", pos);
	}
}

Variable CodeGenerator::evalVariableReference(const VariableReferenceCWeakPtr& variableRef)
{
	if (auto member = variableRef.as<MemberAccess>()) {
		cat::Stack<MemberAccessCWeakPtr> subMembers;
		{
			MemberAccessCWeakPtr parent = member;
			subMembers.push(member);
			while((parent = parent->parent.as<MemberAccess>())) {
				subMembers.push(parent);
			}
		}
		auto parent = subMembers.peek()->parent.as<VariableReference>();
		if (not parent) {
			throw SyntaxError(cat::SW() << "Accessing members of expressions is currently not supported.", variableRef->pos);
		}

		auto variable = getVariable(parent->name, parent->pos);
		auto declType = variable->type();
		auto relAddr = variable->address();
		auto refMode = variable->referenceMode();
		bool isConst = variable->isConst();
		auto isTemporary = false; //

		while (not subMembers.empty()) {
			auto subMember = subMembers.pop();
			auto subMemberVar = declType->scope()->tryGetVariable(subMember->name);
			if (not subMemberVar) {
				throw SyntaxError(cat::SW() << "unknown member '" << subMember->name << "' in object of type " << declType->name() << ".", subMember->pos);
			}


			switch (refMode) {
			case ReferenceMode::STACK_VAL:
				switch (subMemberVar->referenceMode()) {
				case ReferenceMode::STACK_VAL:
					relAddr += subMemberVar->address();
					break;
				case ReferenceMode::STACK_REF:
					throw cat::Exception("invalid referenceMode: STACK_REF cannot be used here");
				case ReferenceMode::HEAP_REF:
					relAddr += subMemberVar->address();
					refMode = ReferenceMode::HEAP_REF;
					break;
				case ReferenceMode::HEAP_VAL:
					if (isTemporary) {
						addInstruction(Instrs::PopVal(subMember->pos));
						isTemporary = false;
					}
					relAddr = subMemberVar->address();
					refMode = ReferenceMode::HEAP_VAL;
					break;
				}
				break;



			case ReferenceMode::STACK_REF:
				switch (subMemberVar->referenceMode()) {
				case ReferenceMode::STACK_VAL:
					if (not isTemporary) {

						addInstruction(Instrs::ReadStackF(frameToStack(relAddr), subMember->pos));     //ReadStackF might leave a zombie, if value is temporary....
					}
					if (subMemberVar->address() != 0) {
						addInstruction(Instrs::PushInt(subMemberVar->address(), subMember->pos));
						addInstruction(Instrs::SubSI(subMember->pos));
					}
					isTemporary = true;
					relAddr = 0;
					break;
				case ReferenceMode::STACK_REF:
					throw cat::Exception("invalid referenceMode: STACK_REF cannot be used here");
				case ReferenceMode::HEAP_REF:
					if (not isTemporary) {
						addInstruction(Instrs::ReadStackF(frameToStack(relAddr), subMember->pos));     //ReadStackF might leave a zombie, if value is temporary....
					}
					addInstruction(Instrs::ReadFR(subMemberVar->address(), subMember->pos));
					isTemporary = true;
					relAddr = 0;
					refMode = ReferenceMode::HEAP_REF;
					break;
				case ReferenceMode::HEAP_VAL:
					if (isTemporary) {
						addInstruction(Instrs::PopVal(subMember->pos));
						isTemporary = false;
					}
					relAddr = subMemberVar->address();
					refMode = ReferenceMode::HEAP_VAL;
					break;
				}
				break;
				throw cat::Exception("invalid referenceMode: STACK_REF cannot be used here");
			case ReferenceMode::HEAP_REF:
				switch (subMemberVar->referenceMode()) {
				case ReferenceMode::STACK_VAL:
					if (not isTemporary) {
						addInstruction(Instrs::ReadStackF(frameToStack(relAddr), subMember->pos));     //ReadStackF might leave a zombie, if value is temporary....
					}
					if (subMemberVar->address() != 0) {
						addInstruction(Instrs::PushInt(subMemberVar->address(), subMember->pos));
						addInstruction(Instrs::AddSI(subMember->pos));
					}
					isTemporary = true;
					relAddr = 0;
					break;
				case ReferenceMode::STACK_REF:
					throw cat::Exception("invalid referenceMode: STACK_REF cannot be used here");
				case ReferenceMode::HEAP_REF:
					if (not isTemporary) {
						addInstruction(Instrs::ReadStackF(frameToStack(relAddr), subMember->pos));     //ReadStackF might leave a zombie, if value is temporary....
					}
					addInstruction(Instrs::ReadFR(subMemberVar->address(), subMember->pos));
					isTemporary = true;
					relAddr = 0;
					break;
				case ReferenceMode::HEAP_VAL:
					if (isTemporary) {
						addInstruction(Instrs::PopVal(subMember->pos));
						isTemporary = false;
					}
					relAddr = subMemberVar->address();
					refMode = ReferenceMode::HEAP_VAL;
					break;
				}
				break;
			case ReferenceMode::HEAP_VAL:
				switch (subMemberVar->referenceMode()) {
				case ReferenceMode::STACK_VAL:
					relAddr += subMemberVar->address();
					break;
				case ReferenceMode::STACK_REF:
					throw cat::Exception("invalid referenceMode: STACK_REF cannot be used here");
				case ReferenceMode::HEAP_REF:
					addInstruction(Instrs::ReadFA(subMemberVar->address(), subMember->pos));
					isTemporary = true;
					relAddr = 0;
					break;
				case ReferenceMode::HEAP_VAL:
					relAddr = subMemberVar->address();
					refMode = ReferenceMode::HEAP_VAL;
					break;
				}
				break;
			}
			declType = subMemberVar->type();
			isConst |= subMemberVar->isConst();
		}
		return Variable(declType, relAddr, refMode, false, isConst, isTemporary);
	} else {
		const auto& name = variableRef->name;
		return *getVariable(name, variableRef->pos);
	}

}

bool CodeGenerator::hasVariable(const std::string& name) const
{
	return tryGetVariable(name) != nullptr;
}

BuiltinFunctionCWeakPtr CodeGenerator::tryGetBuiltinFunction(const std::string& name, const FunctionSignature& signature) const
{
	auto functionsIt = builtinFunctions.find(name);
	if (functionsIt != builtinFunctions.end()) {
		auto overloadsIt = functionsIt->second.find(signature);
		if (overloadsIt != functionsIt->second.end()) {
			return &overloadsIt->second;
		}
	}

	return nullptr;
}

FunctionBaseCWeakPtr CodeGenerator::getFunction(const std::string& name, const FunctionSignature& signature, const Position& pos) const
{
	auto result = tryGetBuiltinFunction(name, signature);
	if (result != nullptr) {
		return result;
	}

	bool hasFoundName = false;
	foreach_c(scope, cat::reversed(scopeStack)) {
		if (auto func = scope->tryGetFunction(name, signature)) {
			return func;
		}
	}
	hasFoundName = true;
	if (hasFoundName) {
		throw SyntaxError(cat::SW() << "no valid overload for function " << name << "(" << signature.asCodeString() << ").", pos);
	} else {
		throw SyntaxError(cat::SW() << "no function named '" << name << "'.", pos);
	}
}

bool CodeGenerator::hasFunction(const std::string& name, const FunctionSignature& signature) const
{
	auto result = tryGetBuiltinFunction(name, signature);
	if (result != nullptr) {
		return true;
	}

	foreach_c(scope, cat::reversed(scopeStack)) {
		if (auto func = scope->tryGetFunction(name, signature)) {
			return true;
		}
	}
	return false;
}

TypeCWeakPtr CodeGenerator::tryGetBuiltinType(const std::string& name) const
{
	auto typesIt = builtinTypes.find(name);
	if (typesIt != builtinTypes.end()) {
		return typesIt->second.getRaw();
	}
	return nullptr;
}

TypeCWeakPtr CodeGenerator::tryGetType(const std::string& name) const
{
	if (auto type = tryGetBuiltinType(name)) {
		return type;
	}


	foreach_c(scope, cat::reversed(scopeStack)) {
		if (auto type = scope->tryGetType(name)) {
			return type;
		}
	}
	return nullptr;
}

TypeCWeakPtr CodeGenerator::getType(const std::string& name, const Position& pos) const
{
	if (auto type = tryGetType(name)) {
		return type;
	} else {
		throw SyntaxError(cat::SW() << "unknown type '" << name << "'.", pos);
	}
}

bool CodeGenerator::hasType(const std::string& name) const
{
	return tryGetType(name) != nullptr;
}

void CodeGenerator::checkType(const TypeCWeakPtr& expectation, const TypeCWeakPtr& reality, const Position& pos) const
{
	if (expectation != reality) {
		throw SyntaxError(cat::SW() << "expected expression returning " << expectation->asCodeString() << ", but got " << reality->asCodeString() << ".", pos);
	}

}

void CodeGenerator::pushScope(bool newStackFrame)
{
	if (newStackFrame) {
		scopeStack.emplace_back(new Scope(0));
	} else {
		auto currentSalt = currentScope()->getFrameSize();
		scopeStack.emplace_back(new Scope(std::move(currentSalt)));
	}
//	saltCntr++;
}

void CodeGenerator::popScope()
{
	scopeStack.pop_back();
}

Address CodeGenerator::frameToStack(Address a) const
{
	return currentScope()->getFrameSize() + tempsOnStack - a - 1;
}

void CodeGenerator::readFromVariable(VariableCWeakPtr variable, const Position& pos)
{
	switch (variable->referenceMode()) {
	case ReferenceMode::STACK_VAL:
		readFromStackFrameF(variable->address(), variable->fixedSize(), pos);
		break;
	case ReferenceMode::STACK_REF:
		Address stackAddress;
		if (not variable->isTemporary()) {
			addInstruction(Instrs::ReadStackF(frameToStack(variable->address()), pos));
			stackAddress = 0;
		} else {
			stackAddress = variable->address();
		}
		readFromStackFrameD(stackAddress, variable->type()->fixedSize(), pos);
		break;
	case ReferenceMode::HEAP_VAL:
		readFromHeapF(variable->address(), variable->type()->fixedSize(), pos);
		break;
	case ReferenceMode::HEAP_REF:
		readFromHeapD(variable->address(), variable->type()->fixedSize(), pos);
		//throw cat::Exception("invalid referenceMode: HEAP_REF cannot be used here");
		break;
	}
}

void CodeGenerator::writeToVariable(VariableCWeakPtr variable, const Position& pos)
{
	switch (variable->referenceMode()) {
	case ReferenceMode::STACK_VAL:
		writeToStackFrameF(variable->address(), variable->fixedSize(), pos);
		break;
	case ReferenceMode::STACK_REF:
		Address stackAddress;
		if (not variable->isTemporary()) {
			addInstruction(Instrs::ReadStackF(frameToStack(variable->address()), pos));
			stackAddress = 0;
		} else {
			stackAddress = variable->address();
		}
		writeToStackFrameD(stackAddress, variable->fixedSize(), pos);
		break;
	case ReferenceMode::HEAP_VAL:
		writeToHeapF(variable->address(), variable->type()->fixedSize(), pos);
		break;
	case ReferenceMode::HEAP_REF:
		writeToHeapD(variable->address(), variable->type()->fixedSize(), pos);
		//throw cat::Exception("invalid referenceMode: HEAP_REF cannot be used here");
		break;
	}
}

void CodeGenerator::readFromStackFrameF(Address relAddr, Address amount, const Position& pos)
{
	const auto stackAddr = frameToStack(relAddr);
	for (auto i = amount; i --> 0;) {
		addInstruction(Instrs::ReadStackF(stackAddr, pos));
	}
}

void CodeGenerator::writeToStackFrameF(Address relAddr, Address amount, const Position& pos)
{
	const auto stackAddr = frameToStack(relAddr) - amount + 1;
	for (auto i = amount; i --> 0;) {
		addInstruction(Instrs::WriteStackF(stackAddr, pos));
	}
}

void CodeGenerator::readFromStackFrameD(Address relAddr, Address amount, const Position& pos)
{
	NGPL_ASSERT(relAddr == 0);
	//auto stackAddrAddr = (currentScope()->getFrameSize() + tempsOnStack - relAddr - 1);
	auto stackAddr = frameToStack(0) + 1; // amount ;//- 1; // amount + 1;
	//addInstruction(Instrs::ReadStackF(stackAddrAddr, pos));
	for (auto i = amount; i --> 0;) {
		addInstruction(Instrs::Dup(0, pos));
		addInstruction(Instrs::ReadStackD(stackAddr, pos));
		addInstruction(Instrs::Swap(pos));
	}
	addInstruction(Instrs::PopVal(pos));
//	for (auto i = amount; i --> 0;) {
//		addInstruction(Instrs::ReadStackD(stackAddr, pos));
//		addInstruction(Instrs::WriteStackF(i+1, pos));
//		stackAddr -= 2;
//	}
}

void CodeGenerator::writeToStackFrameD(Address relAddr, Address amount, const Position& pos)
{
	NGPL_ASSERT(relAddr == 0);
	NGPL_ASSERT2(false, "TBD!");
//	auto stackAddrAddr = (currentScope()->getFrameSize() + tempsOnStack - relAddr);//+1;
//	const auto stackAddr = (currentScope()->getFrameSize() + tempsOnStack) - amount;//+1;
	auto stackAddr = frameToStack(0) + 2;
	for (auto i = amount; i --> 0;) {
		addInstruction(Instrs::Dup(0, pos));
		addInstruction(Instrs::ReadStackF(stackAddr, pos));


//		addInstruction(Instrs::ReadStackF(stackAddrAddr, pos));
//		addInstruction(Instrs::WriteStackD(stackAddr, pos));
//		stackAddrAddr -= 1;
	}
}

void CodeGenerator::readFromHeapF(Address addr, Address amount, const Position& pos)
{
	auto heapAddr = addr;
	for (auto i = amount; i --> 0;) {
		addInstruction(Instrs::ReadFA(heapAddr, pos));
		heapAddr += 1;
	}
}

void CodeGenerator::writeToHeapF(Address addr, Address amount, const Position& pos)
{
	auto heapAddr = addr + amount - 1;
	for (auto i = amount; i --> 0;) {
		addInstruction(Instrs::WriteFA(heapAddr, pos));
		heapAddr -= 1;
	}
}

void CodeGenerator::readFromHeapD(Address relAddr, Address amount, const Position& pos)
{
	auto stackAddrAddr = (currentScope()->getFrameSize() + tempsOnStack - relAddr - 1);
	const auto stackAddr = currentScope()->getFrameSize() + tempsOnStack - 1 + 1;
	for (auto i = amount; i --> 0;) {
		addInstruction(Instrs::ReadStackF(stackAddrAddr, pos));
		addInstruction(Instrs::ReadFR(stackAddr, pos));
		stackAddrAddr += 1;
	}
}

void CodeGenerator::writeToHeapD(Address relAddr, Address amount, const Position& pos)
{
	auto stackAddrAddr = (currentScope()->getFrameSize() + tempsOnStack - relAddr);//+1;
	const auto stackAddr = (currentScope()->getFrameSize() + tempsOnStack) - amount;//+1;
	for (auto i = amount; i --> 0;) {
		addInstruction(Instrs::ReadStackF(stackAddrAddr, pos));
		addInstruction(Instrs::WriteFR(stackAddr, pos));
		stackAddrAddr -= 1;
	}
}

void CodeGenerator::cleanupStack(uint16_t amount, const Position& pos)
{
	NGPL_ASSERT(tempsOnStack == 0);
	for (auto i = amount; i --> 0;) {
		addInstruction(Instrs::PopVal(pos));
	}
	tempsOnStack = 0;
}


}

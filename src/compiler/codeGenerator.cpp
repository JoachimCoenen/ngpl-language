#include "codeGenerator.h"

#include "builtins/builtins.h"

#include "ranges.h"
#include "cat_DynArray.h"

namespace ngpl {

using namespace builtinShorthands;

//    Instruction
using Instrs = intermediate::Instructions;

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
		evalStatement(stmt.weak());
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
		evalStatement(innerDecl.weak());
	}

	unit->setScope(currentScope());
	popScope();

	return unit;
}


TypeReference CodeGenerator::evalLiteral(const LiteralCWeakPtr& literal)
{
	if (auto literalBool = literal.as<LiteralBool>()) {
		addInstruction(Instrs::PushInt(uint64_t(literalBool->get()), literalBool->pos));
		return TypeReference(getType("Bool", literal->pos), {}, false);
	}
	if (auto literalInt = literal.as<LiteralInt>()) {
		addInstruction(Instrs::PushInt(literalInt->get(), literalInt->pos));
		return TypeReference(getType("Int", literal->pos), {}, false);
	}
	if (auto literalString = literal.as<LiteralString>()) {
		addInstruction(Instrs::PushStr(literalString->get(), literalString->pos));
		return  TypeReference(getType("String", literal->pos), {}, false);
	}

	throw SyntaxError(cat::SW() << "Unknown Literal type! (compiler is broken!)'" << literal->getTypeName() << "'.", literal->pos);
}

TypeReference CodeGenerator::evalExpression(const ExpressionCWeakPtr& expr, bool asReference)
{
	uint32_t oldTempsOnStack = tempsOnStack;
	auto result = _evalExpression_(expr, asReference);
	NGPL_ASSERT(tempsOnStack - oldTempsOnStack ==
				(asReference ? 1 : (int64_t)result.fixedSize()));
	return result;
}

TypeReference CodeGenerator::_evalExpression_(const ExpressionCWeakPtr& expr, bool asReference)
{
	if (auto literal = expr.as<Literal>()) {
		NGPL_ASSERT2(not asReference, "cannot reference literals");
		return evalLiteral(literal);
	}

	if (auto variableRef = expr.as<VariableReference>()) {
		auto [variable, isReference] = evalVariableReference(variableRef);
		if (asReference or variable.type().isReference()) {
			refFromVariable(&variable, variableRef->pos);
		} else {
			readFromVariable(&variable, isReference, variableRef->pos);
		}
		return variable.type();
	}

	if (auto funcCall = expr.as<FunctionCall>()) {
		NGPL_ASSERT2(not asReference, "cannot reference FunctionCall");
		const auto& name = funcCall->name;

		TypeReference parentType = NONE_TYPE();
		const bool isMethod = funcCall->parent != nullptr;
		if (isMethod) {
			parentType = evalExpression(funcCall->parent.weak(), asReference=true);
		}

		auto argsTemp = cat::range(funcCall->arguments)
			.map(LAMBDA2(this, v) {return this->evalExpression(v.weak()); })
			.toContainer2<cat::DynArray>();

		CallArgTypes args{{argsTemp.rbegin(), argsTemp.rend()}};

		FunctionBaseCWeakPtr func;
		if (isMethod) {
			func = parentType.scope()->tryGetFunction(name, {std::move(args)});
			if (not func) {
				throw SyntaxError(cat::SW() << "unknown member '" << funcCall->name << "' in object of type " << parentType.asQualifiedCodeString() << ".", funcCall->pos);
			}
		} else {
			func = getFunction(name, {std::move(args)}, funcCall->pos);
		}
		evalFunction(func, funcCall->pos);


		if (isMethod) {
			// cleaning up...
			// current stack Layout:
			// ...|parent|returnValue|
			// desired stack Layout:
			// ...|returnValue|
			auto baseAddr = currentScope()->getFrameSize() + tempsOnStack- func->returnType().fixedSize();

			// REMOVED because 'self' is now HEAP_REF:
			// if (auto selfRef = funcCall->parent.as<VariableReference>()) {
			// 	readFromStackFrameF(baseAddr - parentType->fixedSize(), parentType->fixedSize(), funcCall->pos);
			// 	auto selfVar = evalVariableReference(selfRef);
			// 	writeToVariable(&selfVar, funcCall->pos);
			// }

			// REMOVED because 'self' is now HEAP_REF:
			// readFromStackFrameF(baseAddr, func->returnType()->fixedSize(), funcCall->pos);
			// writeToStackFrameF(baseAddr - parentType->fixedSize(), func->returnType()->fixedSize(), funcCall->pos);
			// for (auto i = parentType->fixedSize(); i --> 0; ) {
			// 	addInstruction(Instrs::PopVal(funcCall->pos));
			// }
		}


		return func->returnType();
	}

	if (auto operCall = expr.as<BinaryOperatorCall>()) {
		NGPL_ASSERT2(not asReference, "cannot reference BinaryOperatorCall");
		const auto& name = operCall->name;
		cat::DynArray<TypeReference> args {
			/*lhs*/ this->evalExpression(operCall->lhs.weak()),
			/*rhs*/ this->evalExpression(operCall->rhs.weak())
		};
		args = {args.rbegin(), args.rend()};
		const auto& func = getFunction(name, std::move(args), operCall->pos);
		evalFunction(func, operCall->pos);

		return func->returnType();
	}
	if (auto operCall = expr.as<UnaryOperatorCall>()) {
	NGPL_ASSERT2(not asReference, "cannot reference UnaryOperatorCall");
	const auto& name = operCall->name;
	cat::DynArray<TypeReference> args {
		this->evalExpression(operCall->operand.weak()),
	};
	const auto& func = getFunction(name, std::move(args), operCall->pos);
	evalFunction(func, operCall->pos);

	return func->returnType();
}

	throw SyntaxError(cat::SW() << "Unknow ExpressinType '" << expr->getTypeName() << "'.", expr->pos);
}

TypeReference CodeGenerator::evalFunction(const FunctionBaseCWeakPtr& func, const Position& pos)
{
	if (auto fn = func.as<Function>()) {
		// handle stack frame:
//		addInstruction(Instrs::PushInt(currentScope().salt, pos));
//		addInstruction(Instrs::ReadFA(0, pos));

//		const auto& add = tryGetBuiltinFunction("+", FunctionSignature{{ GET_BUILIN_TYPE("Int"), GET_BUILIN_TYPE("Int") }});
//		addInstruction(Instrs::Call(&(add->_body), pos));

//		addInstruction(Instrs::WriteFA(0, pos));

		// call function:
		addInstruction(Instrs::Call(&fn.get(), pos));


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
			addInstruction(Instrs::Call(&fn.get(), pos));
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
		for (auto i = type.fixedSize(); i --> 0; ) {
			addInstruction(Instrs::PopVal(expr->pos));
		}
	}

	else if (auto assignment = stmt.as<Assignment>()) {
		auto [variable, isReference] = evalVariableReference(assignment->variable.weak());
		auto type = evalExpression(assignment->expr.weak());
		checkType(variable.type(), type, assignment->expr->pos);

		writeToVariable(&variable, isReference, assignment->pos);
	}

	else if (auto ifControl = stmt.as<IfControl>()) {
		const auto conditionType = evalExpression(ifControl->condition.weak());
		checkType(BOOL_TYPE(), conditionType, ifControl->condition->pos);
		tempsOnStack -= 1;

		itm::IntermediateCodeContainer ifCode;
		codeContainerStack.push_back(&ifCode);
		evalBlock(ifControl->thenBlock.weak());
		codeContainerStack.pop_back();

		itm::IntermediateCodeContainer elseCode;
		if (ifControl->elseBlock) {
			codeContainerStack.push_back(&elseCode);
			evalBlock(ifControl->elseBlock.weak());
			codeContainerStack.pop_back();
		}

		addInstruction(new itm::IntermediateIf(false, std::move(ifCode), std::move(elseCode), ifControl->pos));

	}

	else if (auto whileControl = stmt.as<WhileControl>()) {
		//const auto whileConditionPos = getCurrentPos();
		itm::IntermediateCodeContainer code;
		codeContainerStack.push_back(&code);

		const auto conditionType = evalExpression(whileControl->condition.weak());
		checkType(BOOL_TYPE(), conditionType, whileControl->condition->pos);
		tempsOnStack -= 1;

		itm::IntermediateCodeContainer ifCode;
		codeContainerStack.push_back(&ifCode);
		addInstruction(new itm::IntermediateSpecial(itm::IntermediateSpecialId::BREAK, whileControl->pos));
		codeContainerStack.pop_back();

		itm::IntermediateCodeContainer elseCode;
		addInstruction(new itm::IntermediateIf(true, std::move(ifCode), std::move(elseCode), whileControl->pos));



		evalBlock(whileControl->block.weak());
		addInstruction(new itm::IntermediateSpecial(itm::IntermediateSpecialId::CONTINUE, whileControl->pos));
		codeContainerStack.pop_back();
		addInstruction(new itm::IntermediateLoop(std::move(code), whileControl->pos));
	}

	else if (auto returnStmt = stmt.as<ReturnStatement>()) {
		auto type = evalExpression(returnStmt->expr.weak());
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
		evalStatement(stmt.weak());
	}

	auto oldScope = std::move(*currentScope());
	popScope();
	// cleanup stack:
	cleanupStack(oldScope.getFrameSize() - currentScope()->getFrameSize(), block->pos);
//	for (auto i = oldScope.salt; i --> currentScope().salt;) {
//		addInstruction(Instrs::PopVal(block->pos));
	//	}
}

FunctionSignature CodeGenerator::makeFunctionSignature(FuncDeclarationCWeakPtr funcDecl)
{
	auto parameterTypes =
			cat::range(funcDecl->parameters)
			.map_c(LAMBDA2(&, param) {
				return getTypeRef(param->type.weak());
			})
			.toContainer2<cat::DynArray>();

	TypeReference returnType = NONE_TYPE();
	if (funcDecl->returnType) {
		returnType = getTypeRef(funcDecl->returnType.weak());
	}

	FunctionSignature signature{std::move(parameterTypes), std::move(returnType)};
	return signature;
}

void CodeGenerator::evalFunctionSignature(FuncDeclarationCWeakPtr funcDecl)
{
	foreach_c(param, funcDecl->parameters) {
		const auto& name = param->name;
		if (hasVariable(name)) {
			throw SyntaxError(cat::SW() << "redefinition of '" << name << "'.", param->pos);
		}
		// auto relAddr = currentScope()->addVariable(name, type);
		currentScope()->addVariable(name, new Variable(getTypeRef(param->type.weak()), 0, ReferenceMode::STACK_VAL, true, false));
		// writeToStackFrame(relAddr, type->fixedSize(), param->pos);
	}

	const auto oldFrameSize = currentScope()->getFrameSize();
	{
		TypeReference returnType = NONE_TYPE();
		if (funcDecl->returnType) {
			returnType = getTypeRef(funcDecl->returnType.weak());
		}
		//currentScope()->addVariable("*return", returnType);
		currentScope()->setVariable("*return", new Variable(std::move(returnType), 0, ReferenceMode::STACK_VAL, false, false));
	}

	// make sure there's enough room for the return value:
	const auto newFrameSize = currentScope()->getFrameSize();
	if (newFrameSize > oldFrameSize) {
		NGPL_ASSERT(tempsOnStack == 0);
		for (auto i = oldFrameSize; i < newFrameSize; ++i ) {
			addInstruction(Instrs::PushInt(0, funcDecl->pos));
		}
		tempsOnStack = 0;
	}
}

void CodeGenerator::evalDeclaration(const DeclarationCWeakPtr& decl)
{
	NGPL_ASSERT(tempsOnStack == 0);
	if (auto varDecl = decl.as<VarDeclaration>()) {
		const auto& name = decl->name;
		if (hasVariable(name)) {
			throw SyntaxError(cat::SW() << "redefinition of '" << name << "'.", decl->pos);
		}

		std::optional<TypeReference> type = std::nullopt;
		bool valuesAlreadyOnStack = false;
		if (varDecl->initExpr) {
			NGPL_ASSERT(tempsOnStack == 0);
			valuesAlreadyOnStack = true;
			type = evalExpression(varDecl->initExpr.weak());
			NGPL_ASSERT(tempsOnStack == int64_t(type->fixedSize()));
		}

		std::optional<TypeReference> declType = std::nullopt;
		if (varDecl->type) {
			declType = getTypeRef(varDecl->type.weak());
		}

		if (type and declType) {
			checkType(declType.value(), type.value(), varDecl->initExpr->pos);
		}

		if (not declType) {
			declType = type;
		}
		if (not declType->baseType()->isFinished()) {
			throw SyntaxError(cat::SW() << "Illegal use of unfinished type '" << type->asCodeString() << "'.", varDecl->pos);
		}

		const bool isConst = bool(decl.as<ConstDeclaration>());
		const bool isGlobal = this->isGlobal();
		if (isGlobal) {
			throw SyntaxError(cat::SW() << "global variables not implemented yet.", varDecl->pos);

			auto variable = currentScope()->addVariable(name, new Variable(TypeReference{*declType}, staticHeapSize, ReferenceMode::HEAP_VAL, true, isConst));
			staticHeapSize += variable->type().fixedSize();
			writeToHeapF(variable->address(), variable->type().fixedSize(), varDecl->pos);
			NGPL_ASSERT(tempsOnStack == 0);
		} else {
			allocateStackVariable(name, TypeReference{*declType}, isConst, valuesAlreadyOnStack, varDecl->pos);
		}
	}

	else if (auto funcDecl = decl.as<FuncDeclaration>()) {
		//const auto jmpOverFuncDeclPos = addInstruction(Instrs::Nop(funcDecl->pos));
		//const auto funcEntryPos = getCurrentPos();

		FunctionSignature signature = makeFunctionSignature(funcDecl);

		cat::String reason;
		if (not canAddFunction(funcDecl->name, signature, reason)) {
			throw SyntaxError(reason, decl->pos);
		}

		TypeWeakPtr selfType = typeStack.empty() ? nullptr : typeStack.peek();
		bool isMethod = selfType != nullptr;

		const auto qualifier = isMethod ? selfType->asQualifiedCodeString() + "." : "";
		FunctionWeakPtr function = new Function(funcDecl->name, qualifier, std::move(signature), selfType, false);
		currentScope()->addFunction(&function.get());

		codeContainerStack.push_back(&function->body());
		pushScope(true);


		// handle self reference:
		if (isMethod) {
			currentScope()->addVariable("self", new Variable(TypeReference{selfType, true}, 0, ReferenceMode::STACK_VAL, true, false));
		}
		// handle function Signature:
		evalFunctionSignature(funcDecl);

		// handle function Body:
		evalBlock(funcDecl->block.weak());

		// cleanup stack:
		auto returnVar = currentScope()->tryGetVariable("*return");
		cleanupStack(currentScope()->getFrameSize() - (returnVar->address() + returnVar->fixedSize()), funcDecl->pos);

		// return
		addInstruction(new itm::IntermediateSpecial(itm::IntermediateSpecialId::RETURN, funcDecl->pos));

		//setInstruction(jmpOverFuncDeclPos, Instrs::JumpFA(getCurrentPos(), funcDecl->pos));
		popScope();
		codeContainerStack.pop_back();
	}
	else if (auto typeDecl = decl.as<TypeDeclaration>()) {
		evalTypeDeclaration(typeDecl);
	} else {
		throw SyntaxError("Future syntax not supported yet.", decl->pos);
	}

	NGPL_ASSERT(tempsOnStack == 0);
}

void CodeGenerator::evalTypeDeclaration(const TypeDeclarationCWeakPtr& typeDecl)
{
	const auto& name = typeDecl->name;
	if (hasVariable(name) or hasType(name)) {
		throw SyntaxError(cat::SW() << "redefinition of '" << name << "'.", typeDecl->pos);
	}

	TypeWeakPtr parentType = typeStack.empty() ? nullptr : typeStack.peek();
	const auto qualifier = parentType != nullptr ? parentType->asQualifiedCodeString() + "." : "";

	TypeWeakPtr type = nullptr;
	if (auto structDecl = typeDecl.as<StructDeclaration>()) {
		type = new Type(name, qualifier, 0 , TypeKind::TUPLE_LIKE);
	} else if (auto structDecl = typeDecl.as<ClassDeclaration>()) {
		type = new Type(name, qualifier, 0 , TypeKind::CLASS_LIKE);
	} else {
		throw SyntaxError("Future syntax not supported yet.", typeDecl->pos);
	}

	currentScope()->addType(name, TypePtr(&type.get()));
	codeContainerStack.push_back(&type->body());
	typeStack.push(type);
	pushScope(true);

	foreach_c(innerDecl, typeDecl->members) {
		if (auto varDecl = innerDecl.as<VarDeclaration>()){
			evalDeclaration(innerDecl.weak());
		}
	}

	// copy pointer over:
	type->setScope(currentScope());

	// finalize the type:
	type->finish();
	foreach_c(innerDecl, typeDecl->members) {
		if (not innerDecl.as<VarDeclaration>()) {
			evalDeclaration(innerDecl.weak());
		}
	}

	popScope();
	typeStack.pop();
	codeContainerStack.pop_back();
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

VariableCWeakPtr CodeGenerator::tryGetVariable(const cat::String& name) const
{
	foreach_c(scope, cat::reversed(scopeStack)) {
		if (auto variable = scope->tryGetVariable(name)) {
			return variable;
		}
	}
	return nullptr;
}

VariableCWeakPtr CodeGenerator::getVariable(const cat::String& name, const Position& pos) const
{
	if (auto variable = tryGetVariable(name)) {
		return variable;
	} else {
		throw SyntaxError(cat::SW() << "unknown variable '" << name << "'.", pos);
	}
}

std::pair<Variable, bool> CodeGenerator::evalVariableReference(const VariableReferenceCWeakPtr& variableRef)
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
		bool isReference = variable->isReference();
		bool isConst = variable->isConst();
		auto isTemporary = false; //
		Address offset = variable->fixedOffset();


		while (not subMembers.empty()) {
			auto subMember = subMembers.pop();
			auto subMemberVar = declType.scope()->tryGetVariable(subMember->name);
			if (not subMemberVar) {
				throw SyntaxError(cat::SW() << "unknown member '" << subMember->name << "' in object of type " << declType.asCodeString() << ".", subMember->pos);
			}

			auto subMemberVarType = subMemberVar->type();
			auto subMemberVarIsConst = subMemberVar->isConst();

			switch (refMode) {
			case ReferenceMode::STACK_VAL:
				if (declType.isReference()) {
					switch (subMemberVar->referenceMode()) {
					case ReferenceMode::STACK_VAL:
						if (subMemberVarType.isReference()) {
							if (not isTemporary) {
								addInstruction(Instrs::ReadStackF(frameToStack(relAddr), subMember->pos));     //ReadStackF might leave a zombie, if value is temporary....
							}
							addInstruction(Instrs::ReadFR(subMemberVar->address() + offset, subMember->pos));
							isTemporary = true;
							isReference = true;
							offset = subMemberVar->fixedOffset();
							relAddr = 0;
						} else {
							if (not isTemporary) {
								addInstruction(Instrs::ReadStackF(frameToStack(relAddr), subMember->pos));     //ReadStackF might leave a zombie, if value is temporary....
							}
							if (subMemberVar->address() != 0) {
								//addInstruction(Instrs::IncR(subMemberVar->address(), subMember->pos));
							}
							isTemporary = true;
							isReference = true;
							offset += subMemberVar->address();
							relAddr = 0;
						}
						break;
					case ReferenceMode::HEAP_VAL:
						if (isTemporary) {
							addInstruction(Instrs::PopVal(subMember->pos));
							isTemporary = false;
						}
						isReference = false;
						offset = 0;
						relAddr = subMemberVar->address();
						refMode = ReferenceMode::HEAP_VAL;
						break;
					}
				} else {
					switch (subMemberVar->referenceMode()) {
					case ReferenceMode::STACK_VAL:
						if (subMemberVarType.isReference()) {
							isReference = true;
							offset = subMemberVar->fixedOffset();
							relAddr += subMemberVar->address();
							refMode = ReferenceMode::STACK_VAL;
						} else {
							relAddr += subMemberVar->address();
						}
						break;
					case ReferenceMode::HEAP_VAL:
						if (isTemporary) {
							addInstruction(Instrs::PopVal(subMember->pos));
							isTemporary = false;
						}
						isReference = false;
						relAddr = subMemberVar->address();
						refMode = ReferenceMode::HEAP_VAL;
						break;
					}
				}
				break;
			case ReferenceMode::HEAP_VAL:
				switch (subMemberVar->referenceMode()) {
				case ReferenceMode::STACK_VAL:
					if (subMemberVarType.isReference()) {
						addInstruction(Instrs::ReadFA(subMemberVar->address(), subMember->pos));
						isTemporary = true;
						isReference = true;
						offset = subMemberVar->fixedOffset();
						relAddr = 0;
					} else {
						relAddr += subMemberVar->address();
						isReference = false;
					}
					break;
				case ReferenceMode::HEAP_VAL:
					relAddr = subMemberVar->address();
					refMode = ReferenceMode::HEAP_VAL;
					isReference = false;
					break;
				}
				break;
			}
			declType = subMemberVarType;
			isConst |= subMemberVarIsConst;
		}

		/*
		while (not subMembers.empty()) {
			auto subMember = subMembers.pop();
			auto subMemberVar = declType.scope()->tryGetVariable(subMember->name);
			if (not subMemberVar) {
				throw SyntaxError(cat::SW() << "unknown member '" << subMember->name << "' in object of type " << declType.asCodeString() << ".", subMember->pos);
			}

			switch (refMode) {
			case ReferenceMode::STACK_VAL:
				switch (subMemberVar->referenceMode()) {
				case ReferenceMode::STACK_VAL:
					relAddr += subMemberVar->address();
					break;
				case ReferenceMode::HEAP_REF:
					offset = subMemberVar->fixedOffset();
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

			case ReferenceMode::HEAP_REF:
				switch (subMemberVar->referenceMode()) {
				case ReferenceMode::STACK_VAL:
					if (not isTemporary) {
						addInstruction(Instrs::ReadStackF(frameToStack(relAddr), subMember->pos));     //ReadStackF might leave a zombie, if value is temporary....
					}
					if (subMemberVar->address() != 0) {
						//addInstruction(Instrs::IncR(subMemberVar->address(), subMember->pos));
					}
					isTemporary = true;
					offset += subMemberVar->address();
					relAddr = 0;
					break;
				case ReferenceMode::HEAP_REF:
					if (not isTemporary) {
						addInstruction(Instrs::ReadStackF(frameToStack(relAddr), subMember->pos));     //ReadStackF might leave a zombie, if value is temporary....
					}
					addInstruction(Instrs::ReadFR(subMemberVar->address() + offset, subMember->pos));
					isTemporary = true;
					offset = subMemberVar->fixedOffset();
					relAddr = 0;
					break;
				case ReferenceMode::HEAP_VAL:
					if (isTemporary) {
						addInstruction(Instrs::PopVal(subMember->pos));
						isTemporary = false;
					}
					offset = 0;
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
				case ReferenceMode::HEAP_REF:
					addInstruction(Instrs::ReadFA(subMemberVar->address(), subMember->pos));
					isTemporary = true;
					offset = subMemberVar->fixedOffset();
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
		*/

		return std::make_pair(Variable(std::move(declType), relAddr, refMode, false, isConst, isTemporary, offset), isReference);
	} else {
		const auto& name = variableRef->name;
		return std::make_pair(*getVariable(name, variableRef->pos), false);
	}

}

bool CodeGenerator::hasVariable(const cat::String& name) const
{
	return tryGetVariable(name) != nullptr;
}

VariableCWeakPtr CodeGenerator::allocateStackVariable(const cat::String& name, TypeReference&& type, bool isConst, bool valuesAlreadyOnStack, const Position& pos)
{
	if (not valuesAlreadyOnStack) {
		// we need to init:
		defaultInit(type, pos);
	}
	auto variable = currentScope()->addVariable(name, new Variable(std::move(type), 0, ReferenceMode::STACK_VAL, true, isConst));
	NGPL_ASSERT(tempsOnStack == int64_t(variable->fixedSize()));
	tempsOnStack = 0;
	return variable;
}

void CodeGenerator::defaultInit(const TypeReference& typeRef, const Position& pos) {
	if (typeRef.isReference()) {
		addInstruction(Instrs::PushNullR(pos));
	} else if (not typeRef.baseType()->isBasic()) {
		foreach_c(pair, typeRef.scope()->getVariables()) {
			defaultInit(pair.second->type(), pos);
		}
	} else {
		// we have a basic type:
		if (cat::isAnyOf(typeRef.baseType()->name(), "Bool", "Int")) {
			addInstruction(Instrs::PushInt(uint64_t(0), pos));
		} else if (typeRef.baseType()->name() == "String") {
			addInstruction(Instrs::PushStr("", pos));
		} else {
			NGPL_ASSERT2(false, "unknown basic type " + typeRef.baseType()->name());
		}
	}
}

BuiltinFunctionCWeakPtr CodeGenerator::tryGetBuiltinFunction(const cat::String& name, const CallArgTypes& argTypes)
{
	return builtins.tryGetFunction(name, argTypes).asStatic<BuiltinFunction>();
}

FunctionBaseCWeakPtr CodeGenerator::getFunction(const cat::String& name, const CallArgTypes& argTypes, const Position& pos) const
{
	auto result = tryGetBuiltinFunction(name, argTypes);
	if (result != nullptr) {
		return result;
	}

	FunctionOverloadsCWeakPtr overloads = nullptr;
	foreach_c(scope, cat::reversed(scopeStack)) {
		if ((overloads = scope->tryGetFunctionOverloads(name))) {
			if (auto func = overloads->tryGetOverload(argTypes)) {
				return func;
			}
		}
	}
	if (overloads) {
		auto errorDescr = cat::SW() << "no valid overload for function " << name << "(" << argTypes.asCodeString() << ").";
		errorDescr << "Possible overloads are:";
		errorDescr.incIndent();
		foreach_c(overload, *overloads) {
			errorDescr += cat::nlIndent;
			errorDescr += overload->asCodeString();
		}
		throw SyntaxError(errorDescr, pos);
	} else {
		throw SyntaxError(cat::SW() << "no function named '" << name << "'.", pos);
	}
}

bool CodeGenerator::hasFunction(const cat::String& name, const CallArgTypes& argTypes) const
{
	auto result = tryGetBuiltinFunction(name, argTypes);
	if (result != nullptr) {
		return true;
	}

	foreach_c(scope, cat::reversed(scopeStack)) {
		if (auto func = scope->tryGetFunction(name, argTypes)) {
			return true;
		}
	}
	return false;
}

bool CodeGenerator::canAddFunction(const cat::String& name, const FunctionSignature& signature, cat::String& reasonOut) const
{
	cat::String reason;
	if (not builtins.canAddFunction(name, signature, reasonOut)) {
		return false;
	}

	if (not currentScope()->canAddFunction(name, signature, reasonOut)) {
		return false;
	}
	reasonOut.clear();
	return true;
}

TypeCWeakPtr CodeGenerator::tryGetType(const cat::String& name) const
{
	if (auto type = builtins.tryGetType(name)) {
		return type;
	}

	foreach_c(scope, cat::reversed(scopeStack)) {
		if (auto type = scope->tryGetType(name)) {
			return type;
		}
	}
	return nullptr;
}

TypeCWeakPtr CodeGenerator::getType(const cat::String& name, const Position& pos) const
{
	if (auto type = tryGetType(name)) {
		return type;
	} else {
		throw SyntaxError(cat::SW() << "unknown type '" << name << "'.", pos);
	}
}

bool CodeGenerator::hasType(const cat::String& name) const
{
	return tryGetType(name) != nullptr;
}

TypeReference CodeGenerator::getTypeRef(const cat::String& name, std::vector<TypeReference>&& arguments, bool isReference, const Position& pos) const
{
	auto typeRef = getType(name, pos);
	return TypeReference(typeRef, std::move(arguments), isReference);
}

TypeReference CodeGenerator::getTypeRef(const TypeExprCWeakPtr typeExpr) const
{
	return getTypeRef(
				typeExpr->name,
				range(typeExpr->arguments).map_c(LAMBDA2(&, a){ return getTypeRef(a.weak()); }).toVector(),
				typeExpr->isReference,
				typeExpr->pos);
}

void CodeGenerator::checkType(const TypeReference& expectation, const TypeReference& reality, const Position& pos) const
{
	if (expectation != reality) {
		throw SyntaxError(cat::SW() << "expected expression returning " << expectation.asCodeString() << ", but got " << reality.asCodeString() << ".", pos);
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

void CodeGenerator::refFromVariable(VariableCWeakPtr variable, const Position& pos)
{
	switch (variable->referenceMode()) {
	case ReferenceMode::STACK_VAL: {
		if (variable->isReference()) {
			// everything 's fine already!
			if (not variable->isTemporary()) {
				auto stackPos = frameToStack(variable->address());
				addInstruction(Instrs::ReadStackF(stackPos, pos));
			}
			// NGPL_ASSERT2(variable->isTemporary(), "Cannot handle non temporaries yet");
			// TODO: maybe copy if not temporary?
		} else {
			auto stackPos = frameToStack(variable->address());
			addInstruction(Instrs::PushStackR(stackPos, pos));
		}
	} break;
	case ReferenceMode::HEAP_VAL: {
		auto stackPos = variable->address();
		addInstruction(Instrs::PushGlobalsR(stackPos, pos));
	} break;
	}
}

void CodeGenerator::readFromVariable(VariableCWeakPtr variable, bool isReference, const Position& pos)
{
	switch (variable->referenceMode()) {
	case ReferenceMode::STACK_VAL:
		if (variable->isReference() or isReference) {
			readFromHeapD(variable->address(), variable->type().fixedSize(), variable->fixedOffset(), pos);
			//throw cat::Exception("invalid referenceMode: HEAP_REF cannot be used yet");
		} else {
			readFromStackFrameF(variable->address(), variable->fixedSize(), pos);
		}
		break;
	case ReferenceMode::HEAP_VAL:
		readFromHeapF(variable->address(), variable->type().fixedSize(), pos);
		break;
	}
}

void CodeGenerator::writeToVariable(VariableCWeakPtr variable, bool isReference, const Position& pos)
{
	Address stackAddress;
	switch (variable->referenceMode()) {
	case ReferenceMode::STACK_VAL:
		if (variable->isReference() or isReference) {
			stackAddress = variable->address();
	//		if (not variable->isTemporary()) {
	//			addInstruction(Instrs::ReadStackF(frameToStack(variable->address()), pos));
	//			stackAddress = 0;
	//		} else {
	//			stackAddress = variable->address();
	//		}
			writeToHeapD(stackAddress, variable->type().fixedSize(), variable->fixedOffset(), variable->isTemporary(), pos);
			//throw cat::Exception("invalid referenceMode: HEAP_REF cannot be used yet");
		} else {
			writeToStackFrameF(variable->address(), variable->fixedSize(), pos);
		}
		break;
	case ReferenceMode::HEAP_VAL:
		writeToHeapF(variable->address(), variable->type().fixedSize(), pos);
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

void CodeGenerator::readFromStackFrameD(Address relAddr, Address amount, Address fixedOffset, const Position& pos)
{
	NGPL_ASSERT(relAddr == 0);
	//auto stackAddrAddr = (currentScope()->getFrameSize() + tempsOnStack - relAddr - 1);
	auto stackAddr = frameToStack(0) + 1; // amount ;//- 1; // amount + 1;
	//addInstruction(Instrs::ReadStackF(stackAddrAddr, pos));
	if (amount == 0) {
		addInstruction(Instrs::PopVal(pos));
		return;
	}
	for (auto i = amount; i --> 1;) {
		addInstruction(Instrs::Dup(0, pos));
		addInstruction(Instrs::ReadStackD(stackAddr, pos));
		addInstruction(Instrs::Swap(pos));
	}
	addInstruction(Instrs::ReadStackD(stackAddr, pos));
//	for (auto i = amount; i --> 0;) {
//		addInstruction(Instrs::ReadStackD(stackAddr, pos));
//		addInstruction(Instrs::WriteStackF(i+1, pos));
//		stackAddr -= 2;
//	}
}

void CodeGenerator::writeToStackFrameD(Address relAddr, Address amount, Address fixedOffset, const Position& pos)
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

void CodeGenerator::readFromHeapD(Address relAddr, Address amount, Address fixedOffset, const Position& pos)
{
//	auto stackAddrAddr = (currentScope()->getFrameSize() + tempsOnStack - relAddr - 1);
//	const auto stackAddr = currentScope()->getFrameSize() + tempsOnStack - 1 + 1;
//	for (auto i = amount; i --> 0;) {
//		addInstruction(Instrs::ReadStackF(stackAddrAddr, pos));
//		addInstruction(Instrs::ReadFR(stackAddr, pos));
//		stackAddrAddr += 1;
//	}

	NGPL_ASSERT(relAddr == 0);
	//auto stackAddrAddr = (currentScope()->getFrameSize() + tempsOnStack - relAddr - 1);
	//auto stackAddr = frameToStack(0) + 1; // amount ;//- 1; // amount + 1;
	//addInstruction(Instrs::ReadStackF(stackAddrAddr, pos));
	if (amount == 0) {
		addInstruction(Instrs::PopVal(pos));
		return;
	}
	for (auto i = amount; i --> 1;) {
		addInstruction(Instrs::Dup(0, pos));
		addInstruction(Instrs::ReadFR(fixedOffset, pos));
		addInstruction(Instrs::Swap(pos));
	}
	addInstruction(Instrs::ReadFR(fixedOffset, pos));
}

void CodeGenerator::writeToHeapD(Address relAddr, Address amount, Address fixedOffset, bool isTemprary, const Position& pos)
{
	NGPL_ASSERT(relAddr == 0);
	auto stackAddrAddr = frameToStack(relAddr);  // (currentScope()->getFrameSize() + tempsOnStack - relAddr);//+1;
	//NGPL_ASSERT(stackAddrAddr == amount); // amount+1 maybe?

	if (amount == 0) {
		if (isTemprary) {
			addInstruction(Instrs::PopVal(pos));
		}
		return;
	}
	for (auto i = amount; i --> 1;) {
		addInstruction(Instrs::ReadStackF(stackAddrAddr, pos));
		addInstruction(Instrs::Swap(pos));
		addInstruction(Instrs::WriteFR(i + fixedOffset, pos));
		stackAddrAddr -= 1;
	}
	if (not isTemprary) {
		addInstruction(Instrs::ReadStackF(stackAddrAddr, pos));
		addInstruction(Instrs::Swap(pos));
	}
	addInstruction(Instrs::WriteFR(0 + fixedOffset, pos));
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

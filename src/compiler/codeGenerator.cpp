#include "codeGenerator.h"

#include "intermediateCodeBuilder.h"
#include "builtins/builtins.h"

#include "ranges.h"
#include "cat_DynArray.h"

namespace ngpl {

using namespace builtinShorthands;

//    Instruction
using Instrs = intermediate::Instructions;

}

#define ASSERT_NO_TEMPS_ON_STACK(cb, pos) (cb)->assertNoTempsOnStack(pos);
#define ASSERT_TEMPS_ON_STACK(cb, expectedCount, pos) (cb)->assertTempsOnStack(expectedCount, pos);

namespace ngpl::compiler {

CodeGenerator::CodeGenerator()
{
	//pushScope(true);
	// add the stackFramePtr:
	// currentCodeBuilder()->addVariable("*stackFramePtr", getType("Int", Position()));
}

void CodeGenerator::evalRoot(RootCWeakPtr root)
{
	//addInstruction(Instrs::PushInt(uint64_t(0), root->pos));
	//addInstruction(Instrs::WriteFA(uint64_t(0), root->pos));

	foreach_c(stmt, root->statements) {
		evalStatement(stmt.weak());
	}

	//addInstruction(Instrs::Nop(Position()));
}

UnitPtr CodeGenerator::evalUnitDeclaration(UnitDeclarationCWeakPtr unitDecl)
{
	const auto& name = unitDecl->name;
	const auto& unitNature = unitDecl->unitNature;
	UnitPtr unit = new Unit(name, nullptr, unitNature);
	pushCodeBuilder(unit->body());
	evalBlock(unitDecl->block.weak(), false);

	unit->setScope(currentCodeBuilder()->baseScope());
	popCodeBuilder();
	return unit;
}


TypeReference CodeGenerator::evalLiteral(LiteralCWeakPtr literal)
{
	auto ccb = currentCodeBuilder();
	if (auto literalBool = literal.as<LiteralBool>()) {
		ccb->addInstruction(Instrs::PushInt(uint64_t(literalBool->get()), literalBool->pos));
		return TypeReference(ccb->getType("Bool", literal->pos), {}, 0, false);
	}
	if (auto literalInt = literal.as<LiteralInt>()) {
		ccb->addInstruction(Instrs::PushInt(literalInt->get(), literalInt->pos));
		return TypeReference(ccb->getType("Int", literal->pos), {}, 0, false);
	}
	if (auto literalString = literal.as<LiteralString>()) {
		ccb->addInstruction(Instrs::PushStr(literalString->get(), literalString->pos));
		return  TypeReference(ccb->getType("String", literal->pos), {}, 0, false);
	}
	if (auto literalString = literal.as<LiteralNil>()) {
		ccb->addInstruction(Instrs::PushNullR(literalString->pos));
		return  TypeReference(ccb->getType("Any", literal->pos), {}, 0, true);
	}


	throw SemanticsError(cat::SW() << "Unknown Literal type! (compiler is broken!)'" << literal->getTypeName() << "'.", literal->pos);
}

TypeReference CodeGenerator::evalExpression(ExpressionCWeakPtr expr, bool asRValRef, bool asLValRef)
{
	NGPL_ASSERT(not (asRValRef and asLValRef));
	uint32_t oldTempsOnStack = currentCodeBuilder()->tempsOnStack();
	auto result = _evalExpression_(expr, asRValRef, asLValRef);
	if (asRValRef) {
		ASSERT_TEMPS_ON_STACK(currentCodeBuilder(), oldTempsOnStack + 1, expr->pos);
	} else if (asLValRef) {
		ASSERT_TEMPS_ON_STACK(currentCodeBuilder(), oldTempsOnStack + 1, expr->pos);
	} else {
		ASSERT_TEMPS_ON_STACK(currentCodeBuilder(), oldTempsOnStack + (int64_t)result.fixedSize(), expr->pos);
	}
	return result;
}

TypeReference CodeGenerator::_evalExpression_(ExpressionCWeakPtr expr, bool asRValRef, bool asLValRef)
{
	const bool asReference = asRValRef;
	if (auto literal = expr.as<Literal>()) {
		NGPL_COMPILER_ASSERT2(not asReference, "cannot reference literals", expr->pos);
		return evalLiteral(literal);
	}

	if (auto variableRef = expr.as<VariableReference>()) {
		auto variableAccess = evalVariableReference(variableRef);

		if (asReference) {
			auto resVar = currentCodeBuilder()->refFromVariable(variableAccess, variableRef->pos);
			return resVar.type();
		} else {
			auto resVar = currentCodeBuilder()->readFromVariable2(variableAccess,  variableRef->pos);
			return resVar.type();
		}
	}

	std::optional<TypeReference> returnType;

	if (auto funcCall = expr.as<FunctionCall>()) {
		const auto& name = funcCall->name;

		if (name == "memalloc") {
			std::cout << "memalloc" << std::endl;
		}

		bool isNonBasicCtor = false;
		TypeReference parentType = NONE_TYPE();
		const bool isMethod = funcCall->parent != nullptr;
		if (isMethod) {
			parentType = evalExpression(funcCall->parent.weak(), asRValRef=true);
		} else if (TypeCWeakPtr ctorType = currentCodeBuilder()->tryGetType(funcCall->name)) {
			if (not ctorType->isBasic()) {
				isNonBasicCtor = true;
				currentCodeBuilder()->allocateStackTemporary(TypeReference(ctorType, 0), funcCall->pos);
			}
		}

		auto argsTemp = cat::range(funcCall->arguments)
			.map(LAMBDA2(this, v) {return this->evalFunctionArgument(v.weak()); })
			.toContainer2<cat::DynArray>();

		//CallArgTypes args{{argsTemp.rbegin(), argsTemp.rend()}};
		CallArgs args{std::move(argsTemp)};

		FunctionBaseCWeakPtr func;
		if (isMethod) {
			func = currentCodeBuilder()->getMethod(parentType, name, {std::move(args)}, funcCall->pos);
		} else if (isNonBasicCtor){
			func = currentCodeBuilder()->getCtor(name, {std::move(args)}, funcCall->pos);
		} else {
			func = currentCodeBuilder()->getFunction(name, {std::move(args)}, funcCall->pos);
		}

		currentCodeBuilder()->callFunction(func, funcCall->pos);

		returnType = func->returnType();
	}

	else if (auto operCall = expr.as<BinaryOperatorCall>()) {
		const auto& name = operCall->name;
		cat::DynArray<Argument> args {
			/*lhs*/ Argument(this->evalExpression(operCall->lhs.weak())),
			/*rhs*/ Argument(this->evalExpression(operCall->rhs.weak()))
		};

		const auto& func = currentCodeBuilder()->getFunction(name, std::move(args), operCall->pos);
		currentCodeBuilder()->callFunction(func, operCall->pos);

		returnType = func->returnType();
	}

	else if (auto operCall = expr.as<UnaryOperatorCall>()) {
		const auto& name = operCall->name;

		if (name == "&") {
			// don't use evalExpression(...)! use: evalVariableReference(...)!
			throw SemanticsError(cat::SW() << "Future only!", expr->pos);
		}

		cat::DynArray<Argument> args {
			Argument(this->evalExpression(operCall->operand.weak())),
		};

		if (name == "^") {
			auto& arg = args[0].type();
			if (not arg.isPointer()) {
				throw SemanticsError(cat::SW() << "cannot dereference non-pointer '" << arg.asCodeString() << "'.", expr->pos);
			}
			if (asReference) {
				// do nothin'
				return arg.asPointerLess().asReference();
			} else {
				return currentCodeBuilder()->dereferenceVariable(args[0].type(), operCall->pos);

			}
		}
		const auto& func = currentCodeBuilder()->getFunction(name, std::move(args), operCall->pos);
		currentCodeBuilder()->callFunction(func, operCall->pos);

		returnType = func->returnType();
	}

	else if (auto sizeOfExpr = expr.as<SizeOfExpression>()) {
		std::cout << "sizeOf" << std::endl;
		auto type = getTypeRef(sizeOfExpr->type.weak());

		auto size = type.fixedSize();
		currentCodeBuilder()->addInstruction(Instrs::PushInt(size, sizeOfExpr->pos));
		return TypeReference(currentCodeBuilder()->getType("Int", sizeOfExpr->pos), {}, 0);
	}

	if (not returnType.has_value()) {
		throw SemanticsError(cat::SW() << "Unknow ExpressinType '" << expr->getTypeName() << "'.", expr->pos);
	}


	if (asReference) {
		if (returnType.value().isPointer()) {
			return returnType.value().asPointerLess().asReference();
		} else if (returnType.value().isClass()) {
			// everything's OK!
			return returnType.value().asReference();
		} else if (returnType.value().isReference()) {
			// NOP; everything's OK!
			return returnType.value();
		} else {
			throw SemanticsError(cat::SW() << "Cannot reference temprary vaues '" << expr->getTypeName() << "'.", expr->pos);
		}
	} else {
		return returnType.value();
	}
}

void CodeGenerator::evalStatement(StatementCWeakPtr stmt)
{
	ASSERT_NO_TEMPS_ON_STACK(currentCodeBuilder(), stmt->pos);

	if (auto expr = stmt.as<Expression>()) {
		auto type = evalExpression(expr);
		for (auto i = type.fixedSize(); i --> 0; ) {
			currentCodeBuilder()->addInstruction(Instrs::PopVal(expr->pos));
		}
	}

	else if (auto assignment = stmt.as<Assignment>()) {
		auto variableAccess = evalVariableReference(assignment->variable.weak());
		auto type = evalExpression(assignment->expr.weak());
		checkType(variableAccess.variable().type(), type, assignment->expr->pos);

		currentCodeBuilder()->writeToVariable(variableAccess, assignment->pos);
	}

	else if (auto ifControl = stmt.as<IfControl>()) {
		const auto conditionType = evalExpression(ifControl->condition.weak());
		checkType(BOOL_TYPE(), conditionType, ifControl->condition->pos);

		itm::IntermediateIf* ifInstr = new itm::IntermediateIf(false, {}, {}, ifControl->pos);
		currentCodeBuilder()->addIf(ifInstr);

		pushCodeBuilder(ifInstr->ifCode, false);
		evalBlock(ifControl->thenBlock.weak());
		popCodeBuilder();

		if (ifControl->elseBlock) {
			pushCodeBuilder(ifInstr->elseCode, false);
			evalBlock(ifControl->elseBlock.weak());
			popCodeBuilder();
		}

	}

	else if (auto whileControl = stmt.as<WhileControl>()) {
		//const auto whileConditionPos = getCurrentPos();
		itm::IntermediateCodeContainer code;
		pushCodeBuilder(code, false);

		const auto conditionType = evalExpression(whileControl->condition.weak());
		checkType(BOOL_TYPE(), conditionType, whileControl->condition->pos);

		itm::IntermediateIf* ifInstr = new itm::IntermediateIf(true, {}, {}, whileControl->pos);
		currentCodeBuilder()->addIf(ifInstr);

		pushCodeBuilder(ifInstr->ifCode, false);
		currentCodeBuilder()->addSpecialInstruction(itm::IntermediateSpecialId::BREAK, whileControl->pos);
		popCodeBuilder();

		evalBlock(whileControl->block.weak());
		currentCodeBuilder()->addSpecialInstruction(itm::IntermediateSpecialId::CONTINUE, whileControl->pos);
		popCodeBuilder();
		currentCodeBuilder()->addLoop(new itm::IntermediateLoop(std::move(code), whileControl->pos));
	}

	else if (auto returnStmt = stmt.as<ReturnStatement>()) {
		auto type = evalExpression(returnStmt->expr.weak());
		auto variable = currentCodeBuilder()->getVariable("*return", returnStmt->pos);
		if (functionStack.peek()->isCtor()) {
			//checkType(variable->type(), type, returnStmt->expr->pos);
		} else {
			checkType(variable->type(), type, returnStmt->pos);
			// "save" result:
			currentCodeBuilder()->writeToVariable(IndirectAccess(Variable{*variable}), returnStmt->pos);
		}
		// cleanup stack:
		currentCodeBuilder()->cleanupStackAndReturn(returnStmt->pos);
	}

	else if (auto decl = stmt.as<Declaration>()) {
		evalDeclaration(decl);
	}

	else {
		throw SemanticsError(cat::SW() << "Unknow StatementType '" << stmt->getTypeName() << "'.", stmt->pos);
	}

	ASSERT_NO_TEMPS_ON_STACK(currentCodeBuilder(), stmt->pos);

}

void CodeGenerator::evalBlock(BlockCWeakPtr block, bool pushScope)
{
	if (pushScope) {
		currentCodeBuilder()->pushScope();
	}

	try {
		foreach_c(innerDecl, block->statements) {
			evalStatement(innerDecl.weak());
		}
	} catch (util::debug::AssertionError& ex) {
		currentCodeBuilder()->logError(new WrappingCompileError(ex.makeCopy(), std::nullopt));
	} catch (SyntaxError& ex) {
		currentCodeBuilder()->logError(ex.makeCopy());
		throw ex;
	} catch (SemanticsError& ex) {
		currentCodeBuilder()->logError(ex.makeCopy());
	} catch (CompileError& ex) {
		currentCodeBuilder()->logError(ex.makeCopy());
	}

	if (pushScope) {
		currentCodeBuilder()->popScope(block->pos);
	}
}

Argument CodeGenerator::evalFunctionArgument(ExpressionCWeakPtr expr)
{
	return Argument(evalExpression(expr));
}

FunctionSignature CodeGenerator::makeFunctionSignature(const std::vector<ParamDeclarationPtr>& parametersDecl, TypeReference&& returnType)
{
	auto parameters =
			cat::range(parametersDecl)
			.map_c(LAMBDA2(&, param) {
				return Parameter(param->name, getTypeRef(param->type.weak()));
			})
			.toContainer2<cat::DynArray>();

	FunctionSignature signature{std::move(parameters), std::move(returnType)};
	return signature;
}

void CodeGenerator::evalFunctionSignature(const FunctionSignature& signature, const Position& pos)
{
	foreach_c(param, signature.parameters()) {
		const auto& name = param.name();
		if (currentCodeBuilder()->hasLocalVariable(name)) {
			throw SemanticsError(cat::SW() << "redefinition of '" << name << "'.", pos);
		}
		// auto relAddr = currentCodeBuilder()->addVariable(name, type);
		currentCodeBuilder()->addVariable(name, new Variable(param.type(), 0_fa, ReferenceMode::STACK_VAL, true, false));
		// writeToStackFrame(relAddr, type->fixedSize(), param->pos);
	}

	currentCodeBuilder()->allocateReturnVariable(TypeReference{signature.returnType()}, pos);
}

void CodeGenerator::evalDeclaration(const DeclarationCWeakPtr& decl)
{
	ASSERT_NO_TEMPS_ON_STACK(currentCodeBuilder(), decl->pos);
	if (auto varDecl = decl.as<VarDeclaration>()) {
		evalVarDeclaration(varDecl);
	}
	else if (auto funcDecl = decl.as<FuncDeclaration>()) {
		evalFuncDeclaration(funcDecl);
	}
	else if (auto ctorDecl = decl.as<CtorDeclaration>()) {
		evalCtorDeclaration(ctorDecl);
	}
	else if (auto dtorDecl = decl.as<DtorDeclaration>()) {
		evalDtorDeclaration(dtorDecl);
	}
	else if (auto typeDecl = decl.as<TypeDeclaration>()) {
		evalTypeDeclaration(typeDecl);
	}
	else {
		throw SemanticsError("Future syntax not supported yet.", decl->pos);
	}

	ASSERT_NO_TEMPS_ON_STACK(currentCodeBuilder(), decl->pos);
}

void CodeGenerator::evalVarDeclaration(const VarDeclarationCWeakPtr& varDecl)
{
	const auto& name = varDecl->name;
	cat::String reasonOut;
	if (not currentCodeBuilder()->canAddVariable(name, reasonOut)) {
		throw SemanticsError(reasonOut, varDecl->pos);
	}

	ASSERT_NO_TEMPS_ON_STACK(currentCodeBuilder(), varDecl->pos);
	std::optional<TypeReference> type = std::nullopt;
	bool valuesAlreadyOnStack = false;
	if (varDecl->initExpr) {
		valuesAlreadyOnStack = true;
		type = evalExpression(varDecl->initExpr.weak());
		ASSERT_TEMPS_ON_STACK(currentCodeBuilder(), int64_t(type->fixedSize()), varDecl->pos);
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
	if (not declType->baseType()->isFinished() and not declType->isRepresentedByReference()) {
		throw SemanticsError(cat::SW() << "Illegal use of unfinished type '" << type->asCodeString() << "'.", varDecl->pos);
	}

	const bool isConst = varDecl->isConst;
	const bool isGlobal = this->isGlobal();
	if (isGlobal) {
		currentCodeBuilder()->allocateHeapVariable(name, TypeReference{*declType}, isConst, valuesAlreadyOnStack, varDecl->pos);
//			throw SemanticsError(cat::SW() << "global variables not implemented yet.", varDecl->pos);

//			auto variable = currentCodeBuilder()->addVariable(name, new Variable(TypeReference{*declType}, staticHeapSize, ReferenceMode::HEAP_VAL, true, isConst));
//			staticHeapSize += variable->type().fixedSize();
//			currentCodeBuilder()->writeToVariable(variable, varDecl->pos);
//			ASSERT_NO_TEMPS_ON_STACK(currentCodeBuilder(), varDecl->pos);
	} else {
		currentCodeBuilder()->allocateStackVariable(name, TypeReference{*declType}, isConst, valuesAlreadyOnStack, varDecl->pos);
	}
}

void CodeGenerator::evalFuncDeclaration(const FuncDeclarationCWeakPtr& funcDecl)
{
	// self type for method:
	TypeWeakPtr selfType = typeStack.empty() ? nullptr : typeStack.peek();
	bool isMethod = selfType != nullptr;

	// return type:
	TypeReference returnType = NONE_TYPE();
	if (funcDecl->returnType) {
		returnType = getTypeRef(funcDecl->returnType.weak());
	}
	FunctionSignature signature = makeFunctionSignature(funcDecl->parameters, std::move(returnType));

	const cat::String name = funcDecl->name;

	cat::String reasonOut;
	if (not currentCodeBuilder()->canAddFunction(name, signature, reasonOut)) {
		throw SemanticsError(reasonOut, funcDecl->pos);
	}

	const auto qualifier = isMethod ? selfType->asQualifiedCodeString() + "." : "";
	FunctionWeakPtr function = new Function(name, qualifier, std::move(signature), selfType, false);
	currentCodeBuilder()->addFunction(name, function.getPtr());

	pushCodeBuilder(function->body());
	functionStack.push(function);

	// handle self reference:
	if (isMethod) {
		currentCodeBuilder()->addVariable("self", new Variable(TypeReference{selfType, 0, true}, 0_fa, ReferenceMode::STACK_VAL, true, false));
	}
	// handle function Signature:
	evalFunctionSignature(function->signature(), funcDecl->pos);

	// handle function Body:
	evalBlock(funcDecl->block.weak());

	// cleanup stack and return:
	currentCodeBuilder()->cleanupStackAndReturn(funcDecl->pos);

	functionStack.pop();
	popCodeBuilder();
}

void CodeGenerator::evalCtorDeclaration(const CtorDeclarationCWeakPtr& ctorDecl)
{

	// self type for method:
	TypeWeakPtr selfType = typeStack.empty() ? nullptr : typeStack.peek();
	bool isMethod = selfType != nullptr;

	if (not isMethod) {
		SemanticsError("Constructors are only allowed inside of types.", ctorDecl->pos);
	}
	if (not ctorDecl->name.empty()) {
		throw SemanticsError("Named constructors are not supported yet.", ctorDecl->pos);
	}

	// return type:
	TypeReference returnType = TypeReference(selfType, 0);
	FunctionSignature signature = makeFunctionSignature(ctorDecl->parameters, std::move(returnType));

	const cat::String name = "*ctor";

	cat::String reasonOut;
	if (not currentCodeBuilder()->canAddFunction(name, signature, reasonOut)) {
		throw SemanticsError(reasonOut, ctorDecl->pos);
	}

	const auto qualifier = selfType->asQualifiedCodeString() + ".";
	FunctionWeakPtr ctor = new Function(name, qualifier, std::move(signature), nullptr, false);
	currentCodeBuilder()->addFunction(name, ctor.getPtr());

	pushCodeBuilder(ctor->body());
	functionStack.push(ctor);

	// handle self reference:
	currentCodeBuilder()->addVariable("self", new Variable(TypeReference{selfType, 0, false}, 0_fa, ReferenceMode::STACK_VAL, true, false));

	// handle function Signature:
	evalFunctionSignature(ctor->signature(), ctorDecl->pos);

	// handle function Body:
	if (selfType->isClass()) {
		std::vector<ExpressionPtr> memallocParams;
		memallocParams.push_back(new LiteralInt(selfType->fixedSize(), ctorDecl->pos));
		Assignment assignment(
					new VariableReference("self", ctorDecl->pos),
					new FunctionCall("memalloc", nullptr, std::move(memallocParams), ctorDecl->pos),
					ctorDecl->pos);
		evalStatement(&assignment);
	}
	evalBlock(ctorDecl->block.weak());

	// cleanup stack and return:
	currentCodeBuilder()->cleanupStackAndReturn(ctorDecl->pos);

	functionStack.pop();
	popCodeBuilder();
}

void CodeGenerator::evalDtorDeclaration(const DtorDeclarationCWeakPtr& dtorDecl)
{
	throw SemanticsError("Future syntax not supported yet.", dtorDecl->pos);
}

void CodeGenerator::evalTypeDeclaration(const TypeDeclarationCWeakPtr& typeDecl)
{
	const auto& name = typeDecl->name;
	cat::String reasonOut;
	if (not currentCodeBuilder()->canAddType(name, reasonOut)) {
		throw SemanticsError(reasonOut, typeDecl->pos);
	}

	TypeWeakPtr parentType = typeStack.empty() ? nullptr : typeStack.peek();
	const auto qualifier = parentType != nullptr ? parentType->asQualifiedCodeString() + "." : "";

	TypeWeakPtr type = nullptr;
	if (auto structDecl = typeDecl.as<StructDeclaration>()) {
		type = new Type(name, qualifier, 0 , TypeKind::TUPLE_LIKE);
	} else if (auto structDecl = typeDecl.as<ClassDeclaration>()) {
		type = new Type(name, qualifier, 0 , TypeKind::CLASS_LIKE);
	} else {
		throw SemanticsError("Future syntax not supported yet.", typeDecl->pos);
	}

	currentCodeBuilder()->addType(name, TypePtr(type.getPtr()));
	pushCodeBuilder(type->body());
	typeStack.push(type);

	foreach_c(innerDecl, typeDecl->members) {
		if (auto varDecl = innerDecl.as<VarDeclaration>()){
			evalDeclaration(innerDecl.weak());
		}
	}

	// copy pointer over:
	type->setScope(currentCodeBuilder()->baseScope());

	// finalize the type:
	type->finish();
	foreach_c(innerDecl, typeDecl->members) {
		if (not innerDecl.as<VarDeclaration>()) {
			evalDeclaration(innerDecl.weak());
		}
	}

	typeStack.pop();
	popCodeBuilder();
}


cat::WriterObjectABC& CodeGenerator::toString(cat::WriterObjectABC& s) const
{
//	cat::range(_instructions)
//			.map_c(LAMBDA(instr) { return instr.toString(); })
//			.addSeparator([](){ return "\n"; } )
//			.forEach_c(LAMBDA2(&s, str) { s += str; });
	return s;
}

}

namespace ngpl::compiler {


IndirectAccess CodeGenerator::evalVariableReference(const ExpressionCWeakPtr& variableRef)
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
		auto parent = subMembers.peek()->parent.weak();

		std::optional<IndirectAccess> variable;
		if (auto varRef = parent.as<VariableReference>()) {
			variable = IndirectAccess(Variable(*currentCodeBuilder()->getVariable(varRef->name, parent->pos)));
		} else {
			auto typeRef = evalExpression(parent, true);
			variable = IndirectAccess(currentCodeBuilder()->getTopStackTemporary(typeRef));
		}

		while (not subMembers.empty()) {
			auto subMember = subMembers.pop();
			variable = currentCodeBuilder()->accessMember3(variable.value(), subMember->name, subMember->pos);
		}
		return std::move(variable.value());

	} else if (auto member = variableRef.as<VariableReference>()){
		const auto& name = member->name;
		return IndirectAccess(Variable{*currentCodeBuilder()->getVariable(name, member->pos)});
	} else {
		auto typeRef = evalExpression(variableRef, true);
		auto variable = IndirectAccess(currentCodeBuilder()->getTopStackTemporary(typeRef));
		return variable;
	}
}

IntermediateCodeBuilderWeakPtr CodeGenerator::currentCodeBuilder()
{
	if (_codeBuilderStack.empty()) {
		return nullptr;
	}
	return _codeBuilderStack.peek().asStatic<IntermediateCodeBuilder>();
}

IntermediateCodeBuilderCWeakPtr CodeGenerator::currentCodeBuilder() const
{
	return _codeBuilderStack.peek().asStatic<IntermediateCodeBuilder>();
}

void CodeGenerator::pushCodeBuilder(intermediate::IntermediateCodeContainer& codeContainer, bool newStackFrame)
{
	_codeBuilderStack.push(new IntermediateCodeBuilder(&codeContainer, currentCodeBuilder(), newStackFrame));
}

void CodeGenerator::popCodeBuilder()
{
	_codeBuilderStack.pop();
}


TypeReference CodeGenerator::getTypeRef(const TypeExprCWeakPtr typeExpr) const
{
	return currentCodeBuilder()->getTypeRef(
				typeExpr->name,
				range(typeExpr->arguments).map_c(LAMBDA2(&, a){ return getTypeRef(a.weak()); }).toVector(),
				typeExpr->pointerDepth,
				typeExpr->pos);
}

void CodeGenerator::checkType(const TypeReference& expectation, const TypeReference& reality, const Position& pos) const
{
	if (not reality.isAssignableTo(expectation)) {
		throw SemanticsError(cat::SW() << "expected expression of type " << expectation.asCodeString() << ", but got " << reality.asCodeString() << ".", pos);
	}

}




}

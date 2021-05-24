#include "intermediateCodeBuilder.h"

#include "builtins/builtins.h"

#include "ranges.h"


namespace ngpl {

using namespace ngpl::intermediate;
using Instrs =  ngpl::intermediate::Instructions;

}

namespace ngpl::compiler {

const ScopeCWeakPtr IntermediateCodeBuilder::lastValidScope() const
{
	if (not _scopeStack.empty()) {
		return _scopeStack.peek().weak();
	}
	if (_context == nullptr) {
		return nullptr;
	}
	return _context->lastValidScope();

}

IntermediateCodeBuilder::IntermediateCodeBuilder(IntermediateCodeContainerWeakPtr codeContainer, IntermediateCodeBuilderCWeakPtr context, bool newStackFrame)
	: _codeContainer(codeContainer),
	  _context(context)
{
	pushScope(newStackFrame);
}

void IntermediateCodeBuilder::pushScope(bool newStackFrame)
{
	auto salt = 0_fa;
	if (not newStackFrame) {
		auto scope = lastValidScope();
		if (scope != nullptr) {
			salt = scope->getFrameSize();
		}
	}
	_scopeStack.push(ScopeSharedPtr{{}, salt});
	//return _scopeStack.peek();
}

ScopeSharedPtr IntermediateCodeBuilder::popScope(const Position& pos)
{
	auto oldScope = std::move(*currentScope());
	auto poppedScope = _scopeStack.pop();
	// cleanup stack:
	cleanupStack(oldScope.getFrameSize() - currentScope()->getFrameSize(), pos);
	return poppedScope;
}

ScopeSharedPtr& IntermediateCodeBuilder::baseScope()
{
	return _scopeStack.front();
}

void IntermediateCodeBuilder::logError(CompileErrorPtr&& error)
{
	std::cout << "ERROR: " << error->message() << std::endl;
	_codeContainer->addError(std::move(error));
}

VariableCWeakPtr IntermediateCodeBuilder::tryGetVariable(const cat::String& name) const
{
	foreach_c(scope, cat::recursive(this, LAMBDA(c){ return c->context().getPtr(); })
			  .flatmap(LAMBDA(c){ return cat::reversed(c->scopeStack()); })) {
		if (auto variable = scope->tryGetVariable(name)) {
			return variable;
		}
	}
	return nullptr;
}

VariableCWeakPtr IntermediateCodeBuilder::getVariable(const cat::String& name, const Position& pos) const
{
	if (auto variable = tryGetVariable(name)) {
		return variable;
	} else {
		throw NameError(cat::SW() << "unknown variable '" << name << "'.", pos);
	}
}

bool IntermediateCodeBuilder::hasVariable(const cat::String& name) const
{
	return tryGetVariable(name) != nullptr;
}

bool IntermediateCodeBuilder::hasLocalVariable(const cat::String& name) const
{
	return currentScope()->tryGetVariable(name) != nullptr;
}

bool IntermediateCodeBuilder::canAddVariable(const cat::String& name, cat::String& reasonOut) const
{
	if (hasVariable(name) or hasType(name) or hasFunction(name)) {
		reasonOut = "redefinition of '" + name + "'.";
		return false;
	}
	return true;
}

VariableCWeakPtr IntermediateCodeBuilder::addVariable(const cat::String& name, VariablePtr&& variable)
{
	return currentScope()->addVariable(name, std::move(variable));
}

VariableCWeakPtr IntermediateCodeBuilder::setVariable(const cat::String& name, VariablePtr&& variable)
{
	return currentScope()->setVariable(name, std::move(variable));
}

BuiltinFunctionCWeakPtr IntermediateCodeBuilder::tryGetBuiltinFunction(const cat::String& name, const CallArgs& args)
{
	return builtins.tryGetFunction(name, args).asStatic<BuiltinFunction>();
}

FunctionBaseCWeakPtr IntermediateCodeBuilder::tryGetFunction(const cat::String& name, const CallArgs& args) const
{
	auto result = tryGetBuiltinFunction(name, args);
	if (result != nullptr) {
		return result;
	}

	// functions
	foreach_c(func, cat::recursive(this, LAMBDA(c){ return c->context().getPtr(); })
			  .flatmap(LAMBDA(c){ return cat::reversed(c->scopeStack()); })
			  .map(LAMBDA2(&, s){ return s->tryGetFunction(name, args); })
			  .filter(LAMBDA(f){ return f != nullptr; }))
	{
		return func;
	}

	return nullptr;
}

FunctionBaseCWeakPtr IntermediateCodeBuilder::getFunction(const cat::String& name, const CallArgs& args, const Position& pos) const
{
	std::vector<FunctionOverloadsCWeakPtr> allOverloads;

	// builtin functions
	if (FunctionOverloadsCWeakPtr overloads = builtins.tryGetFunctionOverloads(name)) {
		if (auto func = overloads->tryGetOverload(args)) {
			return func;
		}
		allOverloads.push_back(overloads);
	}

	// functions
	foreach_c(overloads, cat::recursive(this, LAMBDA(c){ return c->context().getPtr(); })
			  .flatmap(LAMBDA(c){ return cat::reversed(c->scopeStack()); })
			  .map(LAMBDA2(name, s){ return s->tryGetFunctionOverloads(name); })
			  .filter(LAMBDA(o){ return o != nullptr; }))
	{
		if (auto func = overloads->tryGetOverload(args)) {
			return func;
		}
		allOverloads.push_back(overloads);
	}

	if (allOverloads.empty()) {
		throw NameError(cat::SW() << "no symbol named '" << name << "'.", pos);
	} else {
		auto errorDescr = cat::SW() << "no valid overload for function " << name << "(" << args.asCodeString() << "). ";
		errorDescr << "Possible overloads are:";
		errorDescr.incIndent();
		foreach_c(overload, cat::range(allOverloads).flatmap(LAMBDA(ovlds){ return cat::range(*ovlds); })) {
			errorDescr += cat::nlIndent;
			errorDescr += overload->asCodeString();
		}
		errorDescr += cat::nlIndent;
		throw NameError(errorDescr, pos);
	}
}

FunctionBaseCWeakPtr IntermediateCodeBuilder::getMethod(const TypeReference& parentType, const cat::String& name, const CallArgs& args, const Position& pos) const
{
	std::vector<FunctionOverloadsCWeakPtr> allOverloads;

	// builtin functions
	if (FunctionOverloadsCWeakPtr overloads = parentType.scope()->tryGetFunctionOverloads(name)) {
		if (auto func = overloads->tryGetOverload(args)) {
			return func;
		}
		allOverloads.push_back(overloads);
	}

	if (allOverloads.empty()) {
		throw NameError(cat::SW() << "no method named '" << name << "' in " << parentType.asQualifiedCodeString() << ".", pos);
	} else {
		auto errorDescr = cat::SW() << "no valid overload for method " << name << "(" << args.asCodeString() << ") in " << parentType.asQualifiedCodeString() << ". ";
		errorDescr << "Possible overloads are:";
		errorDescr.incIndent();
		foreach_c(overload, cat::range(allOverloads).flatmap(LAMBDA(ovlds){ return cat::range(*ovlds); })) {
			errorDescr += cat::nlIndent;
			errorDescr += overload->asCodeString();
		}
		errorDescr += cat::nlIndent;
		throw NameError(errorDescr, pos);
	}
}

bool IntermediateCodeBuilder::hasFunction(const cat::String& name) const
{
	if (builtins.tryGetFunctionOverloads(name)) {
		return true;
	}

	// functions
	if (cat::recursive(this, LAMBDA(c){ return c->context().getPtr(); })
	  .flatmap(LAMBDA(c){ return cat::reversed(c->scopeStack()); })
	  .map(LAMBDA2(&, s){ return s->tryGetFunctionOverloads(name); })
	  .any(LAMBDA(f){ return f != nullptr; })) {
		return true;
	}

	return false;
}

bool IntermediateCodeBuilder::hasFunction(const cat::String& name, const CallArgs& args) const
{
	return tryGetFunction(name, args) != nullptr;
}

bool IntermediateCodeBuilder::canAddFunction(const cat::String& name, const FunctionSignature& signature, cat::String& reasonOut) const
{
	if (hasVariable(name) or hasType(name)) {
		reasonOut = "redefinition of '" + name + "'.";
		return false;
	}

	if (not builtins.canAddFunction(name, signature, reasonOut)) {
		return false;
	}

	if (not currentScope()->canAddFunction(name, signature, reasonOut)) {
		return false;
	}
	reasonOut.clear();
	return true;
}

FunctionBaseWeakPtr IntermediateCodeBuilder::addFunction(const cat::String& name, FunctionBasePtr&& function)
{
	return currentScope()->addFunction(name, std::move(function));
}

FunctionBaseCWeakPtr IntermediateCodeBuilder::tryGetBuiltinCtor(const cat::String& name, const CallArgs& args)
{
	if (auto biType = builtins.tryGetType(name)) {
		return biType->scope()->tryGetFunction("*ctor", args);
	}
	return nullptr;
}

FunctionBaseCWeakPtr IntermediateCodeBuilder::tryGetCtor(const cat::String& name, const CallArgs& args) const
{
	auto result = tryGetBuiltinCtor(name, args);
	if (result != nullptr) {
		return result;
	}

	// constructors
	foreach_c(func, cat::recursive(this, LAMBDA(c){ return c->context().getPtr(); })
			  .flatmap(LAMBDA(c){ return cat::reversed(c->scopeStack()); })
			  .map(LAMBDA2(name, s){ return s->tryGetType(name); })
			  .filter(LAMBDA(t){ return t != nullptr; })
			  .map(LAMBDA2(&, t){ return t->scope()->tryGetFunction("*ctor", args); })
			  .filter(LAMBDA(f){ return f != nullptr; }))
	{
		return func;
	}

	return nullptr;
}

FunctionBaseCWeakPtr IntermediateCodeBuilder::getCtor(const cat::String& name, const CallArgs& args, const Position& pos) const
{
	std::vector<FunctionOverloadsCWeakPtr> allOverloads;

	// constructors
	foreach_c(overloads, cat::recursive(this, LAMBDA(c){ return c->context().getPtr(); })
			  .flatmap(LAMBDA(c){ return cat::reversed(c->scopeStack()); })
			  .map(LAMBDA2(name, s){ return s->tryGetType(name); })
			  .filter(LAMBDA(t){ return t != nullptr; })
			  .map(LAMBDA2(&, t){ return t->scope()->tryGetFunctionOverloads("*ctor"); })
			  .filter(LAMBDA(o){ return o != nullptr; }))
	{
		if (auto func = overloads->tryGetOverload(args)) {
			return func;
		}
		allOverloads.push_back(overloads);
	}

	if (allOverloads.empty()) {
		throw NameError(cat::SW() << "no symbol named '" << name << "'.", pos);
	} else {
		auto errorDescr = cat::SW() << "no valid overload for constructor " << name << "(" << args.asCodeString() << "). ";
		errorDescr << "Possible overloads are:";
		errorDescr.incIndent();
		foreach_c(overload, cat::range(allOverloads).flatmap(LAMBDA(ovlds){ return cat::range(*ovlds); })) {
			errorDescr += cat::nlIndent;
			errorDescr += overload->asCodeString();
		}
		errorDescr += cat::nlIndent;
		throw NameError(errorDescr, pos);
	}
}

bool IntermediateCodeBuilder::hasCtor(const cat::String& name, const CallArgs& args) const
{
	return tryGetCtor(name, args) != nullptr;
}

TypeCWeakPtr IntermediateCodeBuilder::tryGetBuiltinType(const cat::String& name)
{
	return builtins.tryGetType(name);
}

TypeCWeakPtr IntermediateCodeBuilder::tryGetType(const cat::String& name) const
{
	if (auto type = tryGetBuiltinType(name)) {
		return type;
	}

	foreach_c(scope, cat::recursive(this, LAMBDA(c){ return c->context().getPtr(); })
			  .flatmap(LAMBDA(c){ return cat::reversed(c->scopeStack()); })) {
		if (auto type = scope->tryGetType(name)) {
			return type;
		}
	}
	return nullptr;
}

TypeCWeakPtr IntermediateCodeBuilder::getType(const cat::String& name, const Position& pos) const
{
	if (auto type = tryGetType(name)) {
		return type;
	} else {
		throw NameError(cat::SW() << "unknown type '" << name << "'.", pos);
	}
}

bool IntermediateCodeBuilder::hasType(const cat::String& name) const
{
	return tryGetType(name) != nullptr;
}

bool IntermediateCodeBuilder::canAddType(const cat::String& name, cat::String& reasonOut) const
{
	if (hasVariable(name) or hasType(name) or hasFunction(name)) {
		reasonOut = "redefinition of '" + name + "'.";
		return false;
	}
	return true;
}

void IntermediateCodeBuilder::addType(const cat::String& name, TypePtr&& type)
{
	return currentScope()->addType(name, std::move(type));
}

TypeReference IntermediateCodeBuilder::getTypeRef(const cat::String& name, std::vector<TypeReference>&& arguments, int pointerDepth, const Position& pos) const
{
	auto typeRef = getType(name, pos);
	return TypeReference(typeRef, std::move(arguments), pointerDepth, false);
}

InstructionPos IntermediateCodeBuilder::addInstruction(InterInstr&& instr) {
	currentCodeContainer()->instructions.push_back(new IntermediateSimpleInstruction(std::move(instr)));
	_tempsOnStack += intermediate::Instructions::stackDeltaForInstructions[instr.id()];
	return getCurrentPos() - 1;
}

InstructionPos IntermediateCodeBuilder::addIf(IntermediateIfPtr&& instr)
{
	currentCodeContainer()->instructions.push_back(std::move(instr));
	_tempsOnStack -= 1;
	return getCurrentPos() - 1;
}

InstructionPos IntermediateCodeBuilder::addLoop(IntermediateLoopPtr&& instr)
{
	currentCodeContainer()->instructions.push_back(std::move(instr));
	return getCurrentPos() - 1;
}

InstructionPos IntermediateCodeBuilder::addSpecialInstruction(IntermediateSpecialId id, const Position& pos)
{
	currentCodeContainer()->instructions.push_back(new IntermediateSpecial(id, pos));
	return getCurrentPos() - 1;
}

void IntermediateCodeBuilder::cleanupStack(Address amount, const Position& pos)
{
	if (_codeContainer->isBad()) {
		return; // 'cause it's already bad.
	}

	NGPL_COMPILER_ASSERT(_tempsOnStack == 0, pos);
	for (auto i = amount; i --> 0;) {
		addInstruction(Instrs::PopVal(pos));
	}
	_tempsOnStack = 0;
}

void IntermediateCodeBuilder::cleanupStackAndReturn(const Position& pos)
{
	// cleanup stack:
	auto returnVar = tryGetVariable("*return");
	cleanupStack(currentScope()->getFrameSize() - (returnVar->address() + returnVar->fixedSize()), pos);

	// return
	addSpecialInstruction(itm::IntermediateSpecialId::RETURN, pos);
}

void IntermediateCodeBuilder::callFunction(const FunctionBaseCWeakPtr& func, const Position& pos)
{
	if (auto fn = func.as<Function>()) {
		// call function:
		addInstruction(Instrs::Call(fn.getPtr(), pos));
		_tempsOnStack += func->stackDelta();
	} else if (auto fn = func.as<BuiltinFunction>()) {
		if (fn->_instructions.has_value()) {
			foreach_c(instrId, fn->_instructions.value()) {
				addInstruction(InterInstr(instrId, None_{}, pos));
			}
		} else {
			addInstruction(Instrs::Call(fn.getPtr(), pos));
			_tempsOnStack += func->stackDelta();
		}
	}
}

TypeReference IntermediateCodeBuilder::dereferenceVariable(const TypeReference& type, const Position& pos)
{
	NGPL_COMPILER_ASSERT(type.isPointer(), pos);
	NGPL_COMPILER_ASSERT(not type.isReference(), pos);
	NGPL_COMPILER_ASSERT(type.baseType()->name() != "Any" or (type.pointerDepth() > 1), pos);

	auto resultType = type.asPointerLess();
	readFromHeapD(currentStackTop(), resultType.fixedSize(), 0, pos);
	return resultType;

}

IndirectAccess IntermediateCodeBuilder::accessMember3(const IndirectAccess& parentAccess, const cat::String& memberName, const Position& memberPos)
{
	auto& parentVar = parentAccess.variable();
	auto& parentType = parentVar.type();
	auto memberVar = parentType.scope()->tryGetVariable(memberName);
	if (not memberVar) {
		throw SemanticsError(cat::SW() << "unknown member '" << memberName << "' in object of type " << parentType.asCodeString() << ".", memberPos);
	}

	if (parentVar.referenceMode() == ReferenceMode::HEAP_VAL) {
		throw SemanticsError(cat::SW() << "HEAP_VAL not yet supported.", memberPos);
	}

	if (memberVar->referenceMode() == ReferenceMode::HEAP_VAL) {
		throw SemanticsError(cat::SW() << "HEAP_VAL not yet supported.", memberPos);
	}

	if (parentAccess.isIndirect()) {
		if (parentVar.isReferenceOrClass()){
			auto result = accessMember3IndirectPointer(parentAccess, *memberVar, memberPos);
			return result;
		} else {
			auto result = accessMember3IndirectValue(parentAccess, *memberVar, memberPos);
			return result;
		}
	} else {
		if (parentVar.isReferenceOrClass()){
			auto result = accessMember3Pointer(parentVar, *memberVar, memberPos);
			return result;
		} else {
			auto result = accessMember3Value(parentVar, *memberVar, memberPos);
			return result;
		}
	}

}


IndirectAccess IntermediateCodeBuilder::accessMember2(const IndirectAccess& parentAccess, const cat::String& memberName, const Position& memberPos)
{
	auto& parent = parentAccess.variable();
	auto& parentType = parent.type();
	auto subMemberVar = parentType.scope()->tryGetVariable(memberName);
	if (not subMemberVar) {
		throw SemanticsError(cat::SW() << "unknown member '" << memberName << "' in object of type " << parentType.asCodeString() << ".", memberPos);
	}

	if (parent.referenceMode() == ReferenceMode::HEAP_VAL) {
		throw SemanticsError(cat::SW() << "HEAP_VAL not yet supported.", memberPos);
	}

	if (subMemberVar->referenceMode() == ReferenceMode::HEAP_VAL) {
		throw SemanticsError(cat::SW() << "HEAP_VAL not yet supported.", memberPos);
	}

	//declType = subMemberVarType;
	bool isConst = parent.isConst() | subMemberVar->isConst();

	std::optional<IndirectAccess> variableAccess;

	auto parentAddress = parent.address();
	if (parent.isReferenceOrClass()) {
		bool parentIsTemporary = parent.isTemporary();
		if (parentAccess.isIndirect()) {
			if (not parentAccess.isTemporary()) {
				readFromStackFrameF(parentAccess.address(), 1, memberPos);   //ReadStackF might leave a zombie, if value is temporary....
			} else {
				NGPL_COMPILER_ASSERT(frameToStack(parentAccess.address()) == 0_sa, memberPos);
			}
			readFromHeapD(currentStackTop(), 1, Address(parentAddress), memberPos);  // == addInstruction(Instrs::ReadFR(Address(parentAddress), memberPos));
			parentAddress = currentStackTop();
			parentIsTemporary = true;
		}

		if (subMemberVar->isReferenceOrClass()) {
			variableAccess = IndirectAccess(
				parentAddress,
				parentIsTemporary,
				Variable(
					TypeReference{subMemberVar->type()},
					subMemberVar->address() + parent.fixedOffset(),
					ReferenceMode::STACK_VAL,
					false,
					isConst,
					subMemberVar->isTemporary(),
					subMemberVar->fixedOffset()
				)
			);
		} else {
			variableAccess = IndirectAccess(
				parentAddress,
				parentIsTemporary,
				Variable(
					TypeReference{subMemberVar->type()},
					subMemberVar->address() + parent.fixedOffset(),
					ReferenceMode::STACK_VAL,
					false,
					isConst,
					subMemberVar->isTemporary(),
					0
				)
			);
		}
	} else {
		auto variable = subMemberVar->isReferenceOrClass()
			? Variable(
				TypeReference{subMemberVar->type()},
				parentAddress + subMemberVar->address(),
				ReferenceMode::STACK_VAL,
				false,
				isConst,
				parent.isTemporary(),
				subMemberVar->fixedOffset()
			)
			: Variable(
				TypeReference{subMemberVar->type()},
				parentAddress + subMemberVar->address(),
				ReferenceMode::STACK_VAL,
				false,
				isConst,
				parent.isTemporary(),
				0
			);
		if (parentAccess.isIndirect()) {
			variableAccess = IndirectAccess(
				parentAccess.address(),
				parentAccess.isTemporary(),
				std::move(variable)
			);
		} else {
			variableAccess = IndirectAccess(std::move(variable));
		}
	}

	return variableAccess.value();


}

std::pair<Variable, bool> IntermediateCodeBuilder::accessMember(const VariableCWeakPtr parent, bool dereference, const cat::String& memberName, const Position& memberPos)
{
	auto variable = parent;
	auto declType = variable->type();
	auto relAddr = variable->address();
	auto refMode = variable->referenceMode();
	bool isReference = false;  //  BAD
	bool isConst = variable->isConst();
	auto isTemporary = variable->isTemporary(); //
	Address offset = variable->fixedOffset();

	if (dereference) {
		switch (refMode) {
		case ReferenceMode::STACK_VAL:
			//if (not isTemporary) {
			//	addInstruction(Instrs::ReadStackF(frameToStack(relAddr), memberPos));     //ReadStackF might leave a zombie, if value is temporary....
			//}
			addInstruction(Instrs::ReadFR(Address(relAddr) + offset, memberPos)); // TODO: INVESTIGATE is "+ offset" correct??
			break;
		}
	}
	dereference = false;

	auto subMemberVar = declType.scope()->tryGetVariable(memberName);
	if (not subMemberVar) {
		throw SemanticsError(cat::SW() << "unknown member '" << memberName << "' in object of type " << declType.asCodeString() << ".", memberPos);
	}

	auto subMemberVarType = subMemberVar->type();
	auto subMemberVarIsConst = subMemberVar->isConst();

	switch (refMode) {
	case ReferenceMode::STACK_VAL:
		if (declType.isReferenceOrClass()) {
			switch (subMemberVar->referenceMode()) {
			case ReferenceMode::STACK_VAL:
				if (subMemberVarType.isReferenceOrClass()) {
					if (not isTemporary) {
						addInstruction(Instrs::ReadStackF(frameToStack(relAddr), memberPos));     //ReadStackF might leave a zombie, if value is temporary....
					}
					// addInstruction(Instrs::ReadFR(Address(subMemberVar->address()) + offset, memberPos)); // TODO: INVESTIGATE is "+ offset" correct??
					isTemporary = true;
					isReference = true;
					dereference = true;
					offset = subMemberVar->fixedOffset();
					relAddr = currentStackTop();
				} else {
					if (not isTemporary) {
						addInstruction(Instrs::ReadStackF(frameToStack(relAddr), memberPos));     //ReadStackF might leave a zombie, if value is temporary....
					}
					//if (subMemberVar->address() != 0) {
					//	addInstruction(Instrs::IncR(subMemberVar->address(), memberPos));
					//}
					isTemporary = true;
					isReference = true;
					offset += Address(subMemberVar->address());
					relAddr = currentStackTop();
				}
				break;
			case ReferenceMode::HEAP_VAL:
				if (isTemporary) {
					addInstruction(Instrs::PopVal(memberPos));
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
				if (subMemberVarType.isReferenceOrClass()) {
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
					addInstruction(Instrs::PopVal(memberPos));
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
			if (subMemberVarType.isReferenceOrClass()) {
				addInstruction(Instrs::ReadFA(frameToHeap(subMemberVar->address()), memberPos));
				isTemporary = true;
				isReference = true;
				offset = subMemberVar->fixedOffset();
				relAddr = currentStackTop();
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


	return std::make_pair(Variable(std::move(declType), relAddr, refMode, false, isConst, isTemporary, offset), isReference);
}

Variable IntermediateCodeBuilder::readFromVariable2(const IndirectAccess& variableAccess, const Position& pos)
{
	if (variableAccess.isIndirect()) {
		auto& variable = variableAccess.variable();
		NGPL_COMPILER_ASSERT(variable.referenceMode() == ReferenceMode::STACK_VAL, pos);
		if (not variableAccess.isTemporary()) {
			readFromStackFrameF(variableAccess.address(), 1, pos);
		} else {
			NGPL_COMPILER_ASSERT(variableAccess.address() == currentStackTop(), pos);
		}
		readFromHeapD(currentStackTop(), variable.type().fixedSize(), Address(variable.address()), pos);
		return Variable(TypeReference{variable.type()}, currentStackTop(), ReferenceMode::STACK_VAL, false, variable.isConst(), true, variable.fixedOffset());
		//throw cat::Exception("invalid referenceMode: HEAP_REF cannot be used yet");
	} else {
		auto& variable = variableAccess.variable();
		switch (variable.referenceMode()) {
		case ReferenceMode::STACK_VAL:
			readFromStackFrameF(variable.address(), variable.fixedSize(), pos);
			break;
		case ReferenceMode::HEAP_VAL:
			readFromHeapF(variable.address(), variable.type().fixedSize(), pos);
			break;
		}
		auto address = currentStackTop() - variable.fixedSize() + 1;
		return Variable(TypeReference{variable.type()}, address, ReferenceMode::STACK_VAL, false, variable.isConst(), true, 0);
	}

	/*
	switch (variable->referenceMode()) {
	case ReferenceMode::STACK_VAL:
		if (/ *variable->isReferenceOrClass() or* / variableAccess.isIndirect()) {
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


	if (asReference or variable->type().isReferenceOrClass()) {
		refFromVariable(variable, pos);
		return Variable(variable->type().asPointer(), currentStackTop(), ReferenceMode::STACK_VAL, false, variable->isConst(), true, 0);
	} else {
		readFromVariable(variable, dereference, pos);
		auto varType = variable->type().asValue();
		auto address = currentStackTop() - varType.fixedSize() + 1;
		return Variable(std::move(varType), address, ReferenceMode::STACK_VAL, false, variable->isConst(), true, 0);
		}
	*/
}

Variable IntermediateCodeBuilder::refFromVariable(const IndirectAccess& variableAccess, const Position& pos)
{
	auto& variable = variableAccess.variable();
	if (variableAccess.isIndirect()) {
		NGPL_COMPILER_ASSERT(variable.referenceMode() == ReferenceMode::STACK_VAL, pos);
		if (variable.isRepresentedByReference()) { // or maybe isReferenceOrClass() ?
			if (not variableAccess.isTemporary()) {
				readFromStackFrameF(variableAccess.address(), 1, pos);
			} else {
				// everything 's fine already!
				NGPL_COMPILER_ASSERT(variableAccess.address() == currentStackTop(), pos);
			}

			readFromHeapD(currentStackTop(), variable.type().fixedSize(), Address(variable.address()), pos);
			addInstruction(Instrs::PushInt(variable.fixedOffset(), pos));
			addInstruction(Instrs::AddR(pos));

			return Variable(variable.type().asReference(), currentStackTop(), ReferenceMode::STACK_VAL, false, variable.isConst(), true, 0);
		} else {
			if (not variableAccess.isTemporary()) {
				readFromStackFrameF(variableAccess.address(), 1, pos);
			} else {
				// everything 's fine already!
				NGPL_COMPILER_ASSERT(variableAccess.address() == currentStackTop(), pos);
			}

			addInstruction(Instrs::PushInt(Address(variable.address()), pos));
			addInstruction(Instrs::AddR(pos));

			return Variable(variable.type().asReference(), currentStackTop(), ReferenceMode::STACK_VAL, false, variable.isConst(), true, 0);
		}

	} else {
		// ?? TODO?
		switch (variable.referenceMode()) {
		case ReferenceMode::STACK_VAL: {
			if (variable.isRepresentedByReference()) { // or maybe isReferenceOrClass() ?
				// everything 's fine already!
				if (not variable.isTemporary()) {
					readFromStackFrameF(variable.address(), 1, pos);
				} else {
					NGPL_COMPILER_ASSERT(variable.address() == currentStackTop(), pos);
				}
				// NGPL_COMPILER_ASSERT2(variable->isTemporary(), "Cannot handle non temporaries yet", pos);
				// TODO: maybe copy if not temporary?
			} else {
				auto stackPos = frameToStack(variable.address());
				addInstruction(Instrs::PushStackR(Address(stackPos), pos));
			}
		} break;
		case ReferenceMode::HEAP_VAL: {
			auto stackPos = variable.address();
			addInstruction(Instrs::PushGlobalsR(Address(stackPos), pos));
		} break;
		}
	}
	return Variable(variable.type().asReference(), currentStackTop(), ReferenceMode::STACK_VAL, false, variable.isConst(), true, variable.fixedOffset());

}

void IntermediateCodeBuilder::readFromVariable(VariableCWeakPtr variable, bool dereference, const Position& pos)
{
	switch (variable->referenceMode()) {
	case ReferenceMode::STACK_VAL:
		if (/*variable->isReferenceOrClass() or*/ dereference) {
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

void IntermediateCodeBuilder::writeToVariable(const IndirectAccess& variableAccess, const Position& pos)
{
	if (variableAccess.isIndirect()) {
		auto& variable = variableAccess.variable();
		NGPL_COMPILER_ASSERT(variable.referenceMode() == ReferenceMode::STACK_VAL, pos);
		//readFromStackFrameF(variableAccess.address(), 1, pos);
		NGPL_COMPILER_ASSERT(variable.fixedOffset() == 0, pos);
		writeToHeapD(variableAccess.address(), variable.type().fixedSize(), Address(variable.address()), variableAccess.isTemporary(), pos);
//		if (variableAccess.isTemporary()) {
//			addInstruction(Instrs::PopVal(pos));
//		}
		//readFromHeapD(currentStackTop(), variable.type().fixedSize(), Address(variable.address()), pos);
	} else {
		auto& variable = variableAccess.variable();
		const bool dereference = variable.isReference();
		switch (variable.referenceMode()) {
		case ReferenceMode::STACK_VAL:
			if (/*variable->isReferenceOrClass() or*/ dereference) {
				auto frameAddress = variable.address();
		//		if (not variable->isTemporary()) {
		//			addInstruction(Instrs::ReadStackF(frameToStack(variable->address()), pos));
		//			stackAddress = 0;
		//		} else {
		//			stackAddress = variable->address();
		//		}
				writeToHeapD(frameAddress, variable.type().asValue().fixedSize(), variable.fixedOffset(), variable.isTemporary(), pos);
				//throw cat::Exception("invalid referenceMode: HEAP_REF cannot be used yet");
			} else {
				writeToStackFrameF(variable.address(), variable.fixedSize(), pos);
			}
			break;
		case ReferenceMode::HEAP_VAL:
			writeToHeapF(variable.address(), variable.type().fixedSize(), pos);
			break;
		}
	}
}

Variable IntermediateCodeBuilder::getTopStackTemporary(const TypeReference& type)
{
	return Variable(type, currentStackTop() - type.fixedSize(), ReferenceMode::STACK_VAL, false, false, true, 0);
}

void IntermediateCodeBuilder::allocateStackTemporary(const TypeReference& type, const Position& pos)
{
	auto oldTempsOnStack = _tempsOnStack;
	defaultInit(type, pos);
	NGPL_COMPILER_ASSERT(_tempsOnStack == oldTempsOnStack + int64_t(type.fixedSize()), pos);
}

VariableCWeakPtr IntermediateCodeBuilder::allocateStackVariable(const cat::String& name, TypeReference&& type, bool isConst, bool valuesAlreadyOnStack, const Position& pos)
{
	if (not valuesAlreadyOnStack) {
		// we need to init:
		defaultInit(type, pos);
	}
	auto variable = currentScope()->addVariable(name, new Variable(std::move(type), 0_fa, ReferenceMode::STACK_VAL, true, isConst));
	NGPL_COMPILER_ASSERT(_tempsOnStack == int64_t(variable->fixedSize()), pos);
	_tempsOnStack = 0;
	return variable;
}

VariableCWeakPtr IntermediateCodeBuilder::allocateHeapVariable(const cat::String& name, TypeReference&& type, bool isConst, bool valuesAlreadyOnStack, const Position& pos)
{
	NGPL_COMPILER_ASSERT(isGlobal(), pos);
	throw SemanticsError(cat::SW() << "global variables not implemented yet.", pos);
//	const bool isGlobal = this->isGlobal();
//	if (isGlobal) {
//		auto variable = currentScope()->addVariable(name, new Variable(TypeReference{*declType}, 0_fa, ReferenceMode::HEAP_VAL, true, isConst));
//		staticHeapSize += variable->type().fixedSize();
//		writeToHeapF(variable->address(), variable->type().fixedSize(), varDecl->pos);
//		NGPL_COMPILER_ASSERT(tempsOnStack == 0, pos);
//	} else {
//		auto variable = currentScope()->addVariable(name, new Variable(TypeReference{*declType}, 0, ReferenceMode::STACK_VAL, true, isConst));
//		NGPL_COMPILER_ASSERT(tempsOnStack == int64_t(variable->type().fixedSize()) or tempsOnStack == 0, pos);
//		tempsOnStack = 0;
//	}

}

void IntermediateCodeBuilder::allocateReturnVariable(TypeReference&& returnType, const Position& pos)
{
	NGPL_COMPILER_ASSERT(_tempsOnStack == 0, pos);
	const auto oldFrameSize = currentScope()->getFrameSize();
	setVariable("*return", new Variable(std::move(returnType), 0_fa, ReferenceMode::STACK_VAL, false, false));

	// make sure there's enough room for the return value:
	const auto newFrameSize = currentScope()->getFrameSize();
	if (newFrameSize > oldFrameSize) {
		NGPL_COMPILER_ASSERT(_tempsOnStack == 0, pos);
		for (auto i = oldFrameSize; i < newFrameSize; ++i ) {
			addInstruction(Instrs::PushInt(0, pos));
		}
		_tempsOnStack = 0;
	}
}

void IntermediateCodeBuilder::defaultInit(const TypeReference& typeRef, const Position& pos)
{
	if (typeRef.isRepresentedByReference()) {
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
			NGPL_COMPILER_ASSERT2(false, "unknown basic type " + typeRef.baseType()->name(), pos);
		}
	}
}

StackAddr IntermediateCodeBuilder::frameToStack(FrameAddr a) const
{
	return StackAddr(currentStackTop() - a);
}

FrameAddr IntermediateCodeBuilder::stackToFrame(StackAddr a) const
{
	return FrameAddr(currentStackTop() - Address(a));
}

FrameAddr IntermediateCodeBuilder::currentStackTop() const
{
	return currentScope()->getFrameSize() + _tempsOnStack - 1;
}

void IntermediateCodeBuilder::readFromStackFrameF(FrameAddr relAddr, Address amount, const Position& pos)
{
	const auto stackAddr = frameToStack(relAddr);
	for (auto i = amount; i --> 0;) {
		addInstruction(Instrs::ReadStackF(stackAddr, pos));
	}
}

void IntermediateCodeBuilder::writeToStackFrameF(FrameAddr relAddr, Address amount, const Position& pos)
{
	const auto stackAddr = frameToStack(relAddr) - amount + 1;
	for (auto i = amount; i --> 0;) {
		addInstruction(Instrs::WriteStackF(stackAddr, pos));
	}
}

void IntermediateCodeBuilder::readFromStackFrameD(FrameAddr relAddr, Address amount, Address fixedOffset, const Position& pos)
{
	NGPL_COMPILER_ASSERT2(false, "TBD!", pos);
	//auto stackAddrAddr = (currentScope()->getFrameSize() + tempsOnStack - relAddr - 1);
	auto stackAddr = frameToStack(relAddr) + 1; // amount ;//- 1; // amount + 1;
	NGPL_COMPILER_ASSERT(stackAddr == 0_sa, pos);
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

void IntermediateCodeBuilder::writeToStackFrameD(FrameAddr relAddr, Address amount, Address fixedOffset, const Position& pos)
{
	NGPL_COMPILER_ASSERT2(false, "TBD!", pos);
//	auto stackAddrAddr = (currentScope()->getFrameSize() + tempsOnStack - relAddr);//+1;
//	const auto stackAddr = (currentScope()->getFrameSize() + tempsOnStack) - amount;//+1;
	auto stackAddr = frameToStack(relAddr) + 2;
	NGPL_COMPILER_ASSERT(stackAddr == 0_sa, pos);
	for (auto i = amount; i --> 0;) {
		addInstruction(Instrs::Dup(0, pos));
		addInstruction(Instrs::ReadStackF(stackAddr, pos));


//		addInstruction(Instrs::ReadStackF(stackAddrAddr, pos));
//		addInstruction(Instrs::WriteStackD(stackAddr, pos));
//		stackAddrAddr -= 1;
	}
}

void IntermediateCodeBuilder::readFromHeapF(FrameAddr addr, Address amount, const Position& pos)
{
	auto heapAddr = frameToHeap(addr);
	for (auto i = amount; i --> 0;) {
		addInstruction(Instrs::ReadFA(heapAddr, pos));
		heapAddr += 1;
	}
}

void IntermediateCodeBuilder::writeToHeapF(FrameAddr addr, Address amount, const Position& pos)
{
	auto heapAddr = frameToHeap(addr) + amount - 1;
	for (auto i = amount; i --> 0;) {
		addInstruction(Instrs::WriteFA(heapAddr, pos));
		heapAddr -= 1;
	}
}

void IntermediateCodeBuilder::readFromHeapD(FrameAddr relAddr, Address amount, Address fixedOffset, const Position& pos)
{
//	auto stackAddrAddr = (currentScope()->getFrameSize() + tempsOnStack - relAddr - 1);
//	const auto stackAddr = currentScope()->getFrameSize() + tempsOnStack - 1 + 1;
//	for (auto i = amount; i --> 0;) {
//		addInstruction(Instrs::ReadStackF(stackAddrAddr, pos));
//		addInstruction(Instrs::ReadFR(stackAddr, pos));
//		stackAddrAddr += 1;
//	}

	auto stackAddrAddr = frameToStack(relAddr);
	NGPL_COMPILER_ASSERT(stackAddrAddr == 0_sa, pos);
	//auto stackAddr = frameToStack(0) + 1; // amount ;//- 1; // amount + 1;
	//addInstruction(Instrs::ReadStackF(stackAddrAddr, pos));
	if (amount == 0) {
		addInstruction(Instrs::PopVal(pos));
		return;
	}

	for (auto i = amount; i --> 1;) {
		addInstruction(Instrs::Dup(0, pos));
		addInstruction(Instrs::ReadFR(fixedOffset+(amount-1-i), pos));
		addInstruction(Instrs::Swap(pos));
	}
	addInstruction(Instrs::ReadFR(fixedOffset+amount-1, pos));
}

void IntermediateCodeBuilder::writeToHeapD(FrameAddr relAddr, Address amount, Address fixedOffset, bool isTemprary, const Position& pos)
{
	auto stackAddrAddr = frameToStack(relAddr);  // (currentScope()->getFrameSize() + tempsOnStack - relAddr);//+1;
	NGPL_COMPILER_ASSERT(isTemprary ? stackAddrAddr - (amount-1 +1 + 1) == 0_sa : true, pos);
	//NGPL_COMPILER_ASSERT(stackAddrAddr == amount, pos); // amount+1 maybe?

	if (amount == 0) {
		if (isTemprary) {
			addInstruction(Instrs::PopVal(pos));
		}
		return;
	}
	for (auto i = amount; i --> 1;) {
		addInstruction(Instrs::ReadStackF(stackAddrAddr, pos));
		//addInstruction(Instrs::Swap(pos));
		addInstruction(Instrs::WriteFR(i + fixedOffset, pos));
		stackAddrAddr -= 1;
	}
	if (isTemprary) {
		addInstruction(Instrs::Swap(pos));
	} else {
		addInstruction(Instrs::ReadStackF(stackAddrAddr, pos));
		//addInstruction(Instrs::Swap(pos));
	}
	addInstruction(Instrs::WriteFR(0 + fixedOffset, pos));
}

IndirectAccess IntermediateCodeBuilder::accessMember3IndirectValue(const IndirectAccess& parentAccess, const Variable& memberVar, const Position& memberPos)
{
	NGPL_COMPILER_ASSERT(parentAccess.isIndirect(), memberPos);
	auto& parentVar = parentAccess.variable();
	bool isConst = parentVar.isConst() | memberVar.isConst();

	NGPL_COMPILER_ASSERT(not parentVar.isTemporary(), memberPos);
	NGPL_COMPILER_ASSERT(not memberVar.isTemporary(), memberPos);
	auto variableAccess = IndirectAccess(
		parentAccess.address(),
		parentAccess.isTemporary(),
		Variable(
			memberVar.type(),
			parentVar.address() + memberVar.address(),
			ReferenceMode::STACK_VAL,
			false,
			isConst,
			parentVar.isTemporary(),
			memberVar.fixedOffset()
		)
	);
	return variableAccess;
}

IndirectAccess IntermediateCodeBuilder::accessMember3IndirectPointer(const IndirectAccess& parentAccess, const Variable& memberVar, const Position& memberPos)
{
	NGPL_COMPILER_ASSERT(parentAccess.isIndirect(), memberPos);
	auto& parentVar = parentAccess.variable();

	bool isConst = parentVar.isConst() | memberVar.isConst();

	if (not parentAccess.isTemporary()) {
		readFromStackFrameF(parentAccess.address(), 1, memberPos);   //ReadStackF might leave a zombie, if value is temporary....
	} else {
		NGPL_COMPILER_ASSERT(parentAccess.address() == currentStackTop(), memberPos);
	}
	readFromHeapD(currentStackTop(), 1, Address(parentVar.address()), memberPos);  // == addInstruction(Instrs::ReadFR(Address(parentAddress), memberPos));
	const auto iAccessAddress = currentStackTop();
	const bool iAccessIsTemporary = true;

	NGPL_COMPILER_ASSERT(not parentVar.isTemporary(), memberPos);
	NGPL_COMPILER_ASSERT(not memberVar.isTemporary(), memberPos);
	auto variableAccess = IndirectAccess(
		iAccessAddress,
		iAccessIsTemporary,
		Variable(
			memberVar.type(),
			memberVar.address() + parentVar.fixedOffset(),
			ReferenceMode::STACK_VAL,
			false,
			isConst,
			parentVar.isTemporary(),
			memberVar.fixedOffset()
		)
	);
	return variableAccess;
}

IndirectAccess IntermediateCodeBuilder::accessMember3Value(const Variable& parentVar, const Variable& memberVar, const Position& memberPos)
{
	bool isConst = parentVar.isConst() | memberVar.isConst();

	NGPL_COMPILER_ASSERT(not parentVar.isTemporary(), memberPos);
	NGPL_COMPILER_ASSERT(not memberVar.isTemporary(), memberPos);
	auto variableAccess = IndirectAccess(Variable(
		memberVar.type(),
		parentVar.address() + memberVar.address(),
		ReferenceMode::STACK_VAL,
		false,
		isConst,
		parentVar.isTemporary(),
		memberVar.fixedOffset()
	));
	return variableAccess;
}

IndirectAccess IntermediateCodeBuilder::accessMember3Pointer(const Variable& parentVar, const Variable& memberVar, const Position& memberPos)
{
	bool isConst = parentVar.isConst() | memberVar.isConst();

	NGPL_COMPILER_ASSERT(not memberVar.isTemporary(), memberPos);
	auto variableAccess = IndirectAccess(
		parentVar.address(),
		parentVar.isTemporary(),
		Variable(
			memberVar.type(),
			memberVar.address() + parentVar.fixedOffset(),
			ReferenceMode::STACK_VAL,
			false,
			isConst,
			parentVar.isTemporary(),
			memberVar.fixedOffset()
		)
	);
	return variableAccess;
}


} // namespace ngpl::compiler

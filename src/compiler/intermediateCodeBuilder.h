#ifndef NGPL_INTERMEDIATE_INTERMEDIATECODEBUILDER_H
#define NGPL_INTERMEDIATE_INTERMEDIATECODEBUILDER_H

#include "syntaxError.h"
#include "intermediate/intermediateCode.h"
#include "language/function.h"
#include "language/scope.h"
#include "language/type.h"


namespace {
namespace itm = ngpl::intermediate;
}

namespace ngpl::compiler {

using InstructionPos = int64_t;


class NameError: public SemanticsError
{
public:
	NameError(const cat::String& message, const Position& pos): SemanticsError(message, pos) {}
};


PTRS_FOR_CLASS(IntermediateCodeBuilder);
class IntermediateCodeBuilder: public NGPLPrivateBase
{
private:
	using InterInstr = intermediate::IntermediateSimpleInstruction;
private:
	itm::IntermediateCodeContainerWeakPtr _codeContainer;
	IntermediateCodeBuilderCWeakPtr _context;
	//cat::WeakPtr<const cat::Stack<ScopeSharedPtr>> _context;
	int64_t _tempsOnStack = 0;
	cat::Stack<ScopeSharedPtr> _scopeStack;//{Scope("0ROOT")};


protected:
	inline itm::IntermediateCodeContainerWeakPtr currentCodeContainer() { return _codeContainer; }
	inline itm::IntermediateCodeContainerCWeakPtr currentCodeContainer() const { return _codeContainer; }

	inline const ScopeSharedPtr& currentScope() { return _scopeStack.peek(); }
	inline const ScopeCWeakPtr currentScope() const { return _scopeStack.peek().weak(); }
	const ScopeCWeakPtr lastValidScope() const;

	inline const cat::Stack<ScopeSharedPtr>& scopeStack() const { return _scopeStack; }
	inline const IntermediateCodeBuilderCWeakPtr context() const { return _context; }

public:
	IntermediateCodeBuilder() = delete;
	IntermediateCodeBuilder(itm::IntermediateCodeContainerWeakPtr codeContainer, IntermediateCodeBuilderCWeakPtr context, bool newStackFrame);

	void pushScope(bool newStackFrame = false);
	ScopeSharedPtr popScope(const Position& pos);
	ScopeSharedPtr& baseScope();

	int64_t tempsOnStack() const { return _tempsOnStack;}

	void logError(CompileErrorPtr&& error);

	// variable, type & function getters and setters etc.:
public:

	VariableCWeakPtr tryGetVariable(const cat::String& name) const;
	VariableCWeakPtr getVariable(const cat::String& name, const Position& pos) const;
	bool hasVariable(const cat::String& name) const;
	bool hasLocalVariable(const cat::String& name) const;
	bool canAddVariable(const cat::String& name, cat::String& reasonOut) const;

	VariableCWeakPtr addVariable(const cat::String& name, VariablePtr&& variable);
	VariableCWeakPtr setVariable(const cat::String& name, VariablePtr&& variable);

	static BuiltinFunctionCWeakPtr tryGetBuiltinFunction(const cat::String& name, const CallArgs& args);
	FunctionBaseCWeakPtr tryGetFunction(const cat::String& name, const CallArgs& args) const;
	FunctionBaseCWeakPtr getFunction(const cat::String& name, const CallArgs& args, const Position& pos) const;
	FunctionBaseCWeakPtr getMethod(const TypeReference& parentType, const cat::String& name, const CallArgs& args, const Position& pos) const;
	bool hasFunction(const cat::String& name) const;
	bool hasFunction(const cat::String& name, const CallArgs& args) const;
	bool canAddFunction(const cat::String& name, const FunctionSignature& signature, cat::String& reasonOut) const;

	FunctionBaseWeakPtr addFunction(const cat::String& name, FunctionBasePtr&& function);

	static FunctionBaseCWeakPtr tryGetBuiltinCtor(const cat::String& name, const CallArgs& args);
	FunctionBaseCWeakPtr tryGetCtor(const cat::String& name, const CallArgs& args) const;
	FunctionBaseCWeakPtr getCtor(const cat::String& name, const CallArgs& args, const Position& pos) const;
	bool hasCtor(const cat::String& name, const CallArgs& args) const;

	static TypeCWeakPtr tryGetBuiltinType(const cat::String& name);
	TypeCWeakPtr tryGetType(const cat::String& name) const;
	TypeCWeakPtr getType(const cat::String& name, const Position& pos) const;
	bool hasType(const cat::String& name) const;
	bool canAddType(const cat::String& name, cat::String& reasonOut) const;

	void addType(const cat::String& name, TypePtr&& type);

	TypeReference getTypeRef(const cat::String& name, std::vector<TypeReference>&& arguments, int pointerDepth, const Position& pos) const;
	inline TypeReference getTypeRef(const cat::String& name, int pointerDepth, const Position& pos) const { return getTypeRef(name, {}, pointerDepth, pos); };
	inline TypeReference getTypeRef(const cat::String& name, const Position& pos) const { return getTypeRef(name, 0, pos); }

	// instruction generating methods:
public:
	inline void assertNoTempsOnStack(const Position& pos) { NGPL_COMPILER_ASSERT(_tempsOnStack == 0, pos); }
	inline void assertTempsOnStack(int64_t expectedCount, const Position& pos) { NGPL_COMPILER_ASSERT(_tempsOnStack == expectedCount, pos); }

	inline InstructionPos getCurrentPos() const {
		return currentCodeContainer()->instructions.size();
	}

	InstructionPos addInstruction(InterInstr&& instr);
	InstructionPos addIf(itm::IntermediateIfPtr&& instr);
	InstructionPos addLoop(itm::IntermediateLoopPtr&& instr);
	InstructionPos addSpecialInstruction(itm::IntermediateSpecialId id, const Position& pos);

	void cleanupStack(Address amount, const Position& pos);
	void cleanupStackAndReturn(const Position& pos);

	void callFunction(const FunctionBaseCWeakPtr& func, const Position& pos);

	TypeReference dereferenceVariable(const TypeReference& type, const Position& pos);

	IndirectAccess accessMember3(const IndirectAccess& parentAccess, const cat::String& memberName, const Position& memberPos);
	IndirectAccess accessMember2(const IndirectAccess& parentAccess, const cat::String& memberName, const Position& memberPos);
	std::pair<Variable, bool> accessMember(const VariableCWeakPtr parent, bool dereference, const cat::String& memberName, const Position& memberPos);

	/**
	 * @brief readFromVariable2
	 * @param variableAccess
	 * @param pos
	 * @return
	 */
	Variable readFromVariable2(const IndirectAccess& variableAccess, const Position& pos);

	/**
	 * @brief puts a reference to the vriable on top of the stack
	 * @param variableAccess
	 * @param pos
	 */
	Variable refFromVariable(const IndirectAccess& variableAccess, const Position& pos);

	void readFromVariable(VariableCWeakPtr variable, bool dereference, const Position& pos);
	inline void readFromVariable(VariableCWeakPtr variable, const Position& pos) { return readFromVariable(variable, false, pos); }
	void writeToVariable(const IndirectAccess& variableAccess, const Position& pos);
	inline void writeToVariable(VariableCWeakPtr variable, const Position& pos) { return writeToVariable(IndirectAccess(Variable{*variable}), pos); }

	Variable getTopStackTemporary(const TypeReference& type);
	void allocateStackTemporary(const TypeReference& type, const Position& pos);
	VariableCWeakPtr allocateStackVariable(const cat::String& name, TypeReference&& type, bool isConst, bool valuesAlreadyOnStack, const Position& pos);
	VariableCWeakPtr allocateHeapVariable(const cat::String& name, TypeReference&& type, bool isConst, bool valuesAlreadyOnStack, const Position& pos);

	void allocateReturnVariable(TypeReference&& returnType, const Position& pos);

	void defaultInit(const TypeReference& typeRef, const Position& pos);


private:
	// Address Tramsformations:
	/**
	 * @brief frameToStack
	 * @param a an Address in callFrame-space (as is needed for variables)
	 * @return that address in stack space (as is needed for the instructions)
	 */
	StackAddr frameToStack(FrameAddr a) const;
	/**
	 * @brief stackToFrame
	 * @param a an Address in stack space (as is needed for the instructions)
	 * @return that address in callFrame-space (as is needed for variables)
	 */
	FrameAddr stackToFrame(StackAddr a) const;
	/**
	 * @brief frameToHeap
	 * @param a an Address in callFrame-space (as is needed for variables)
	 * @return that address in stack space (as is needed for the instructions)
	 */
	HeapAddr frameToHeap(FrameAddr a) const { return HeapAddr(Address(a));}
//	/**
//	 * @brief heapToFrame
//	 * @param a an Address in stack space (as is needed for the instructions)
//	 * @return that address in callFrame-space (as is needed for variables)
//	 */
//	FrameAddr heapToFrame(HeapAddr a) const { return FrameAddr(Address(a));}

	FrameAddr currentStackTop() const;

	void readFromStackFrameF(FrameAddr relAddr, Address amount, const Position& pos);
	void writeToStackFrameF(FrameAddr relAddr, Address amount, const Position& pos);

	void readFromStackFrameD(FrameAddr relAddr, Address amount, Address fixedOffset, const Position& pos);
	void writeToStackFrameD(FrameAddr relAddr, Address amount, Address fixedOffset, const Position& pos);

	void readFromHeapF(FrameAddr addr, Address amount, const Position& pos);
	void writeToHeapF(FrameAddr addr, Address amount, const Position& pos);

	void readFromHeapD(FrameAddr relAddr, Address amount, Address fixedOffset, const Position& pos);
	void writeToHeapD(FrameAddr relAddr, Address amount, Address fixedOffset, bool isTemprary, const Position& pos);


	bool isGlobal() const { return _scopeStack.size() == 1 and _context == nullptr; }

	// Member Access:

	IndirectAccess accessMember3IndirectValue(const IndirectAccess& parentAccess, const Variable& memberVar, const Position& memberPos);
	IndirectAccess accessMember3IndirectPointer(const IndirectAccess& parentAccess, const Variable& memberVar, const Position& memberPos);
	IndirectAccess accessMember3Value(const Variable& parentVar, const Variable& memberVar, const Position& memberPos);
	IndirectAccess accessMember3Pointer(const Variable& parentVar, const Variable& memberVar, const Position& memberPos);
};

} // namespace ngpl::compiler

#endif // NGPL_INTERMEDIATE_INTERMEDIATECODEBUILDER_H

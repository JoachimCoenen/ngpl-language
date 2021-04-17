#ifndef CODEGENERATOR_H
#define CODEGENERATOR_H

#include "intermediate/intermediateCode.h"
#include "vm/value.h"
#include "vm/vm_util.h"
#include "language/ast.h"

#include "language/function.h"
#include "language/scope.h"
#include "syntaxError.h"
#include "language/type.h"
#include "language/unit.h"

#include "cat_string.h"

#include <functional>
#include <map>
#include <unordered_map>

namespace {
namespace itm = ngpl::intermediate;
}

namespace ngpl {

using BuiltinTypesMap = std::unordered_map<cat::String, TypeCPtr>;
using BuiltinFunctionsMap = std::unordered_map<cat::String, FunctionOverloads>;

class CodeGenerator
{
public:
	CodeGenerator();

	void evalRoot(const RootCWeakPtr& root);
	UnitPtr evalUnitDeclaration(const UnitDeclarationCWeakPtr& unitDecl);


	TypeReference evalLiteral(const LiteralCWeakPtr& literal);
	TypeReference evalExpression(const ExpressionCWeakPtr& expr, bool asReference = false);
private:
	TypeReference _evalExpression_(const ExpressionCWeakPtr& expr, bool asReference = false);
public:
	TypeReference evalFunction(const FunctionBaseCWeakPtr& func, const Position& pos);
	void evalStatement(const StatementCWeakPtr& stmt);
	void evalBlock(const BlockCWeakPtr& block);

	FunctionSignature makeFunctionSignature(FuncDeclarationCWeakPtr funcDecl);
	void evalFunctionSignature(FuncDeclarationCWeakPtr funcDecl);
	void evalDeclaration(const DeclarationCWeakPtr& decl);
	void evalTypeDeclaration(const TypeDeclarationCWeakPtr& typeDecl);

	void writeBuiltinFunctions();

	cat::WriterObjectABC& toString(cat::WriterObjectABC& s) const;

	//std::vector<Instruction> _instructions;
	std::vector<ScopeSharedPtr> scopeStack;//{Scope("0ROOT")};
	std::vector<itm::IntermediateCodeContainerWeakPtr> codeContainerStack;
	cat::Stack<TypeWeakPtr> typeStack;
	int64_t tempsOnStack = 0;

	bool isGlobal() const { return scopeStack.size() == 1; }
	int64_t staticHeapSize = 0;


protected:
	using InterInstr = intermediate::IntermediateSimpleInstruction;

	VariableCWeakPtr tryGetVariable(const cat::String& name) const;
	VariableCWeakPtr getVariable(const cat::String& name, const Position& pos) const;
	std::pair<Variable, bool> evalVariableReference(const VariableReferenceCWeakPtr& variable);
	bool hasVariable(const cat::String& name) const;

	VariableCWeakPtr allocateStackVariable(const cat::String& name, TypeReference&& type, bool isConst, bool valuesAlreadyOnStack, const Position& pos);
	/*VariableCWeakPtr allocateHeapVariable(const cat::String& name, TypeReference&& type, ReferenceMode referenceMode, bool isConst)
	{
		const bool isGlobal = this->isGlobal();
		if (isGlobal) {
			auto variable = currentScope()->addVariable(name, new Variable(TypeReference{*declType}, staticHeapSize, ReferenceMode::HEAP_VAL, true, isConst));
			staticHeapSize += variable->type().fixedSize();
			writeToHeapF(variable->address(), variable->type().fixedSize(), varDecl->pos);
			NGPL_ASSERT(tempsOnStack == 0);
		} else {
			auto variable = currentScope()->addVariable(name, new Variable(TypeReference{*declType}, 0, ReferenceMode::STACK_VAL, true, isConst));
			NGPL_ASSERT(tempsOnStack == int64_t(variable->type().fixedSize()) or tempsOnStack == 0);
			tempsOnStack = 0;
		}

	}*/

	void defaultInit(const TypeReference& typeRef, const Position& pos);

	static BuiltinFunctionCWeakPtr tryGetBuiltinFunction(const cat::String& name, const CallArgTypes& argTypes);
	FunctionBaseCWeakPtr getFunction(const cat::String& name, const CallArgTypes& argTypes, const Position& pos) const;
	bool hasFunction(const cat::String& name, const CallArgTypes& argTypes) const;
	bool canAddFunction(const cat::String& name, const FunctionSignature& signature, cat::String& reasonOut) const;

	static TypeCWeakPtr tryGetBuiltinType(const cat::String& name);
	TypeCWeakPtr tryGetType(const cat::String& name) const;
	TypeCWeakPtr getType(const cat::String& name, const Position& pos) const;
	bool hasType(const cat::String& name) const;
	TypeReference getTypeRef(const cat::String& name, std::vector<TypeReference>&& arguments, bool isReference, const Position& pos) const;
	inline TypeReference getTypeRef(const cat::String& name, bool isReference, const Position& pos) const {
		return getTypeRef(name, {}, isReference, pos);
	};
	inline TypeReference getTypeRef(const cat::String& name, const Position& pos) const {
		return getTypeRef(name, false, pos);
	}

	TypeReference getTypeRef(const TypeExprCWeakPtr typeExpr) const;



	void checkType(const TypeReference& expectation, const TypeReference& reality, const Position& pos) const;

	void pushScope(bool newStackFrame = false);
	void popScope();
	inline const ScopeSharedPtr currentScope() { return scopeStack.back(); }
	inline const ScopeCWeakPtr currentScope() const { return scopeStack.back().weak(); }
	inline itm::IntermediateCodeContainerWeakPtr& currentInstructioins() { return codeContainerStack.back(); }
	inline const itm::IntermediateCodeContainerWeakPtr& currentInstructioins() const { return codeContainerStack.back(); }

	// Address Tramsformations:
	/**
	 * @brief frameToStack
	 * @param a an Address in callFrame-space
	 * @return that address in stack space (as is needed for the instructions)
	 */
	Address frameToStack(Address a) const;

	void refFromVariable(VariableCWeakPtr variable, const Position& pos);

	void readFromVariable(VariableCWeakPtr variable, bool isReference, const Position& pos);
	inline void readFromVariable(VariableCWeakPtr variable, const Position& pos) { return readFromVariable(variable, false, pos); }
	void writeToVariable(VariableCWeakPtr variable, bool isReference, const Position& pos);
	inline void writeToVariable(VariableCWeakPtr variable, const Position& pos) { return writeToVariable(variable, false, pos); }

	void readFromStackFrameF(Address relAddr, Address amount, const Position& pos);
	void writeToStackFrameF(Address relAddr, Address amount, const Position& pos);

	void readFromStackFrameD(Address relAddr, Address amount, Address fixedOffset, const Position& pos);
	void writeToStackFrameD(Address relAddr, Address amount, Address fixedOffset, const Position& pos);

	void readFromHeapF(Address addr, Address amount, const Position& pos);
	void writeToHeapF(Address addr, Address amount, const Position& pos);

	void readFromHeapD(Address relAddr, Address amount, Address fixedOffset, const Position& pos);
	void writeToHeapD(Address relAddr, Address amount, Address fixedOffset, bool isTemprary, const Position& pos);

	void cleanupStack(uint16_t amount, const Position& pos);

	inline InstructionPos addInstruction(InterInstr&& instr) {
		currentInstructioins()->instructions.push_back(new itm::IntermediateSimpleInstruction(std::move(instr)));
		tempsOnStack += intermediate::Instructions::stackDeltaForInstructions[instr.id()];
		return getCurrentPos() - 1;
	}

	inline InstructionPos addInstruction(itm::IntermediateInstructionPtr&& instr) {
		currentInstructioins()->instructions.push_back(std::move(instr));
		//tempsOnStack += ngpl::Instructions::stackDeltaForInstructions[instr.id()];
		return getCurrentPos() - 1;
	}

	inline void setInstruction(const InstructionPos& pos, InterInstr&& instr) {
		currentInstructioins()->instructions[pos] = new itm::IntermediateSimpleInstruction(std::move(instr));
	}

//	inline const Instruction& getInstruction(const InstructionPos& pos) const {
//		return currentInstructioins()->instructions[pos];
//	}

	inline InstructionPos getCurrentPos() const {
		return currentInstructioins()->instructions.size();
	}


//    static const std::map<cat::String, std::function<Value(const std::vector<Value>&)>> globalFunctions_;
//    static const std::map<cat::String, std::function<Value(const Value&, const Value&)>> biOperators;
//    static const std::map<cat::String, std::function<Value(const Value&)>> unaryOperators;

};

}
#endif // CODEGENERATOR_H

#ifndef CODEGENERATOR_H
#define CODEGENERATOR_H

#include "intermediate/intermediateCode.h"
#include "../vm/value.h"
#include "../vm/vm_util.h"
#include "../language/ast.h"

#include "function.h"
#include "scope.h"
#include "syntaxError.h"
#include "type.h"
#include "unit.h"

#include <functional>
#include <string>
#include <map>
#include <unordered_map>

namespace {
namespace itm = ngpl::intermediate;
}

namespace ngpl {

class CodeGenerator
{
public:
	CodeGenerator();

	void evalRoot(const RootCWeakPtr& root);
	UnitPtr evalUnitDeclaration(const UnitDeclarationCWeakPtr& unitDecl);


	TypeCWeakPtr evalLiteral(const LiteralCWeakPtr& literal);
	TypeCWeakPtr evalExpression(const ExpressionCWeakPtr& expr);
	TypeCWeakPtr evalFunction(const FunctionBaseCWeakPtr& func, const Position& pos);
	void evalStatement(const StatementCWeakPtr& stmt);
	void evalBlock(const BlockCWeakPtr& block);

	void evalDeclaration(const DeclarationCWeakPtr& decl);

	void writeBuiltinFunctions();

	cat::WriterObjectABC& toString(cat::WriterObjectABC& s) const;

	//std::vector<Instruction> _instructions;
	std::vector<ScopePtr> scopeStack;//{Scope("0ROOT")};
	std::vector<itm::IntermediateCodeContainerWeakPtr> codeContainerStack;
	cat::Stack<TypeWeakPtr> typeStack;
	int64_t tempsOnStack = 0;

	bool isGlobal() const { return scopeStack.size() == 1; }
	int64_t staticHeapSize = 0;

	static const std::unordered_map<std::string, TypeCPtr> builtinTypes;

	static const std::unordered_map<std::string, std::unordered_map<FunctionSignature, BuiltinFunction>> builtinFunctions;

protected:
	using InterInstr = intermediate::IntermediateSimpleInstruction;

	VariableCWeakPtr tryGetVariable(const std::string& name) const;
	VariableCWeakPtr getVariable(const std::string& name, const Position& pos) const;
	Variable evalVariableReference(const VariableReferenceCWeakPtr& variable);
	bool hasVariable(const std::string& name) const;

	BuiltinFunctionCWeakPtr tryGetBuiltinFunction(const std::string& name, const FunctionSignature& signature) const;
	FunctionBaseCWeakPtr getFunction(const std::string& name, const FunctionSignature& signature, const Position& pos) const;
	bool hasFunction(const std::string& name, const FunctionSignature& signature) const;

	TypeCWeakPtr tryGetBuiltinType(const std::string& name) const;
	TypeCWeakPtr tryGetType(const std::string& name) const;
	TypeCWeakPtr getType(const std::string& name, const Position& pos) const;
	bool hasType(const std::string& name) const;



	void checkType(const TypeCWeakPtr& expectation, const TypeCWeakPtr& reality, const Position& pos) const;

	void pushScope(bool newStackFrame = false);
	void popScope();
	inline ScopeWeakPtr currentScope() { return scopeStack.back().getRaw(); }
	inline const ScopeCWeakPtr currentScope() const { return scopeStack.back().getRaw(); }
	inline itm::IntermediateCodeContainerWeakPtr& currentInstructioins() { return codeContainerStack.back(); }
	inline const itm::IntermediateCodeContainerWeakPtr& currentInstructioins() const { return codeContainerStack.back(); }

	// Address Tramsformations:
	/**
	 * @brief frameToStack
	 * @param a an Address in callFrame-space
	 * @return that address in stack space (as is needed for the instructions)
	 */
	Address frameToStack(Address a) const;


	void readFromVariable(VariableCWeakPtr variable, const Position& pos);
	void writeToVariable(VariableCWeakPtr variable, const Position& pos);

	void readFromStackFrameF(Address relAddr, Address amount, const Position& pos);
	void writeToStackFrameF(Address relAddr, Address amount, const Position& pos);

	void readFromStackFrameD(Address relAddr, Address amount, const Position& pos);
	void writeToStackFrameD(Address relAddr, Address amount, const Position& pos);

	void readFromHeapF(Address addr, Address amount, const Position& pos);
	void writeToHeapF(Address addr, Address amount, const Position& pos);

	void readFromHeapD(Address relAddr, Address amount, const Position& pos);
	void writeToHeapD(Address relAddr, Address amount, const Position& pos);

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


//    static const std::map<std::string, std::function<Value(const std::vector<Value>&)>> globalFunctions_;
//    static const std::map<std::string, std::function<Value(const Value&, const Value&)>> biOperators;
//    static const std::map<std::string, std::function<Value(const Value&)>> unaryOperators;

};

}
#endif // CODEGENERATOR_H

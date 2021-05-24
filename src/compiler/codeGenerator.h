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

namespace ngpl::compiler {
// forward declaration:
PTRS_FOR_CLASS(IntermediateCodeBuilder);
//class IntermediateCodeBuilder;

class CodeGenerator
{
private:
	//std::vector<Instruction> _instructions;
	//std::vector<itm::IntermediateCodeContainerWeakPtr> codeContainerStack;
	cat::Stack<ScopeSharedPtr> _context;
	cat::Stack<NGPLPrivateBasePtr> _codeBuilderStack;
	cat::Stack<TypeWeakPtr> typeStack;
	cat::Stack<FunctionWeakPtr> functionStack;
	bool _isInCtor = false;
	FrameAddr staticHeapSize = 0_fa;
public:
	CodeGenerator();

	void evalRoot(RootCWeakPtr root);
	UnitPtr evalUnitDeclaration(UnitDeclarationCWeakPtr unitDecl);


	TypeReference evalLiteral(LiteralCWeakPtr literal);
	TypeReference evalExpression(ExpressionCWeakPtr expr, bool asRValRef = false, bool asLValRef = false);
private:
	TypeReference _evalExpression_(ExpressionCWeakPtr expr, bool asRValRef = false, bool asLValRef = false);
public:
	void evalStatement(StatementCWeakPtr stmt);
	void evalBlock(BlockCWeakPtr block, bool pushScope = true);

	Argument evalFunctionArgument(ExpressionCWeakPtr expr);

	FunctionSignature makeFunctionSignature(const std::vector<ParamDeclarationPtr>& parametersDecl, TypeReference&& returnType);
	void evalFunctionSignature(const FunctionSignature& signature, const Position& pos);
	void evalDeclaration(const DeclarationCWeakPtr& decl);
	void evalVarDeclaration(const VarDeclarationCWeakPtr& varDecl);
	void evalFuncDeclaration(const FuncDeclarationCWeakPtr& funcDecl);
	void evalCtorDeclaration(const CtorDeclarationCWeakPtr& ctorDecl);
	void evalDtorDeclaration(const DtorDeclarationCWeakPtr& dtorDecl);
	void evalTypeDeclaration(const TypeDeclarationCWeakPtr& typeDecl);

	cat::WriterObjectABC& toString(cat::WriterObjectABC& s) const;


	bool isGlobal() const { return false; }


protected:
	using InterInstr = intermediate::IntermediateSimpleInstruction;

	IndirectAccess evalVariableReference(const ExpressionCWeakPtr& variable);

	IntermediateCodeBuilderWeakPtr currentCodeBuilder();
	IntermediateCodeBuilderCWeakPtr currentCodeBuilder() const;
	void pushCodeBuilder(itm::IntermediateCodeContainer& codeContainer, bool newStackFrame = true);
	void popCodeBuilder();

	ScopeSharedPtr& currentD() { return _context.peek(); }
	ScopeCWeakPtr currentContext() const { return _context.peek().weak(); }
//	void pushContext(ScopeSharedPtr& scope) {}
//	void popCodeBuilder();


	TypeReference getTypeRef(const TypeExprCWeakPtr typeExpr) const;

	void checkType(const TypeReference& expectation, const TypeReference& reality, const Position& pos) const;

//    static const std::map<cat::String, std::function<Value(const std::vector<Value>&)>> globalFunctions_;
//    static const std::map<cat::String, std::function<Value(const Value&, const Value&)>> biOperators;
//    static const std::map<cat::String, std::function<Value(const Value&)>> unaryOperators;

};

}
#endif // CODEGENERATOR_H

#ifndef NODE_H
#define NODE_H

#include "position.h"
#include "unitNature.h"

#include "util/types.h"

#include "catPointers.h"
#include "ranges.h"
#include "cat_string.h"
#include "toStringUtils.h"
#include "cat_typing.h"

#include <vector>

namespace ngpl {

struct IFormattable {
	virtual cat::String getTypeName() const { return "IFormattable";}
	virtual cat::WriterObjectABC& _formatVal(cat::WriterObjectABC& s) const = 0;

protected:
	auto _getMembersTuple() const {
		return std::make_tuple();
	}

	auto _getLocalMembersTuple() const {
		return std::make_tuple();
	}
};

	inline cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const IFormattable& v) {
	return v._formatVal(s);
	}


#define STRUCT_WITH_PTR(Cls)\
	PTRS_FOR_STRUCT(Cls)    \
	struct Cls

}

namespace ngpl {


#define NON_COPY_FORMATTABLE(Cls, Supertype)				\
	Cls(const Cls &) = delete;					\
	Cls& operator = (const Cls &) = delete;		\
	cat::String getTypeName() const override {	\
		static_assert (std::is_same_v<const Cls, typeof (*this)>);	\
		return #Cls;							\
	}											\
	auto _getMembersTuple() const {				\
		return std::tuple_cat(Supertype::_getMembersTuple(), _getLocalMembersTuple());	\
	}											\
	cat::WriterObjectABC& _formatVal(cat::WriterObjectABC& s) const override {	\
		s += getTypeName();											\
		auto tuple = _getMembersTuple();		\
		formatTupleLike2(s, tuple, {"(", ")"}, cat::_formatFuncKwArg, true);	\
		return s;								\
	}

PTRS_FOR_STRUCT(Node);
struct Node : public ngpl::IFormattable {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple();
	}
	NON_COPY_FORMATTABLE(Node, ngpl::IFormattable)


	Node(const Position pos)
		: ngpl::IFormattable(), pos(pos)
	{};
	virtual ~Node() {}

	Position pos;

};


PTRS_FOR_STRUCT(Statement);
struct Statement : public Node {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple();
	}
	NON_COPY_FORMATTABLE(Statement, Node);
	Statement(const Position& pos) 
		: Node(pos) 
	{ };

};


PTRS_FOR_STRUCT(Root);
struct Root : public Node {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple(
			MEMBER_PAIR(*this, statements)
		);
	}
	NON_COPY_FORMATTABLE(Root, Node);
	std::vector<StatementPtr> statements;

	Root(const Position& pos) : Node(pos), statements() {}
	Root(std::vector<StatementPtr>&& statements, const Position& pos)
		: Node(pos), statements(std::move(statements)) 
	{}

};


PTRS_FOR_STRUCT(Expression);
struct Expression : public Statement {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple();
	}
	NON_COPY_FORMATTABLE(Expression, Statement);
	Expression(const Position& pos) 
		: Statement(pos)
	{ };
};


PTRS_FOR_STRUCT(Literal);
struct Literal : public Expression {
	NON_COPY_FORMATTABLE(Literal, Expression);

	Literal(const Position& pos) 
		: Expression(pos) 
	{}

	// auto get() const = 0;
};


PTRS_FOR_STRUCT(LiteralBool);
struct LiteralBool : public Literal {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple(
			MEMBER_PAIR(*this, value)
		);
	}
	NON_COPY_FORMATTABLE(LiteralBool, Literal);
	bool value;

	LiteralBool(bool v, const Position& pos)
		: Literal(pos), value(v)
	{}

	auto get() const {
		return value;
	}
};


PTRS_FOR_STRUCT(LiteralInt);
struct LiteralInt : public Literal {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple(
			MEMBER_PAIR(*this, value)
		);
	}
	NON_COPY_FORMATTABLE(LiteralInt, Literal);
	int64_t value;

	LiteralInt(int64_t v, const Position& pos)
		: Literal(pos), value(v)
	{}

	auto get() const {
		return value;
	}
};


PTRS_FOR_STRUCT(LiteralString);
struct LiteralString : public Literal {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple(
			MEMBER_PAIR(*this, value)
		);
	}
	NON_COPY_FORMATTABLE(LiteralString, Literal);
	cat::String value;

	LiteralString(const cat::String& v, const Position& pos)
		: Literal(pos), value(v) 
	{}
	LiteralString(cat::String&& v, const Position& pos)
		: Literal(pos), value(std::move(v)) 
	{}

	const auto& get() const {
		return value;
	}
};


PTRS_FOR_STRUCT(LiteralNil);
struct LiteralNil : public Literal {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple(
		);
	}
	NON_COPY_FORMATTABLE(LiteralNil, Literal);

	LiteralNil(const Position& pos)
		: Literal(pos)
	{}
};


PTRS_FOR_STRUCT(VariableReference);
struct VariableReference : public Expression {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple(
			MEMBER_PAIR(*this, name)
		);
	}
	NON_COPY_FORMATTABLE(VariableReference, Expression);
	cat::String name;

	VariableReference(cat::String&& name, const Position& pos)
	   : Expression(pos), name(std::move(name)) {}
};


PTRS_FOR_STRUCT(MemberAccess);
struct MemberAccess : public VariableReference {
   auto _getLocalMembersTuple() const {
	   return cat::makeCRefTuple(
		   // MEMBER_PAIR(*this, name)
	   );
   }
   NON_COPY_FORMATTABLE(MemberAccess, VariableReference);
	ExpressionPtr parent;

	MemberAccess(cat::String&& name, ExpressionPtr&& parent, const Position& pos)
	  : VariableReference(std::move(name), pos), parent(std::move(parent)) {}
};


PTRS_FOR_STRUCT(FunctionCall);
struct FunctionCall : public Expression {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple(
			MEMBER_PAIR(*this, name),
			MEMBER_PAIR(*this, parent),
			MEMBER_PAIR(*this, arguments)
		);
	}
	NON_COPY_FORMATTABLE(FunctionCall, Expression);
	cat::String name;
	ExpressionPtr parent;
	std::vector<ExpressionPtr> arguments;

	FunctionCall(cat::String&& name, ExpressionPtr&& parent, std::vector<ExpressionPtr>&& args, const Position& pos)
	   : Expression(pos), name(std::move(name)), parent(std::move(parent)), arguments(std::move(args)) {}
};


PTRS_FOR_STRUCT(UnaryOperatorCall);
struct UnaryOperatorCall : public Expression {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple(
			MEMBER_PAIR(*this, name),
			MEMBER_PAIR(*this, operand)
		);
	}
	NON_COPY_FORMATTABLE(UnaryOperatorCall, Expression);
	cat::String name;
	ExpressionPtr operand;

	UnaryOperatorCall(cat::String&& name, ExpressionPtr&& operand, const Position& pos)
		: Expression(pos), name(std::move(name)), operand(std::move(operand)) {}
};


PTRS_FOR_STRUCT(BinaryOperatorCall);
struct BinaryOperatorCall : public Expression {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple(
			MEMBER_PAIR(*this, name),
			MEMBER_PAIR(*this, lhs),
			MEMBER_PAIR(*this, rhs)
		);
	}
	NON_COPY_FORMATTABLE(BinaryOperatorCall, Expression);
	cat::String name;
	ExpressionPtr lhs;
	ExpressionPtr rhs;

	BinaryOperatorCall(cat::String&& name, ExpressionPtr&& lhs, ExpressionPtr&& rhs, const Position& pos)
		: Expression(pos), name(std::move(name)), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

	static int getPrecedence(const cat::String& name);
	int getPrecedence() const { return getPrecedence(name); }
};


PTRS_FOR_STRUCT(TypeExpr);
struct TypeExpr : public Node {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple(
			MEMBER_PAIR(*this, name),
			MEMBER_PAIR(*this, isPointer),
			MEMBER_PAIR(*this, arguments)
		);
	}
	NON_COPY_FORMATTABLE(TypeExpr, Node);
	TypeExpr(cat::String&& name, bool isPointer, std::vector<TypeExprPtr>&& arguments, const Position& pos)
		: Node(pos), name(std::move(name)), isPointer(isPointer), arguments(std::move(arguments)) { };

	cat::String name;
	bool isPointer;
	std::vector<TypeExprPtr> arguments;
};


PTRS_FOR_STRUCT(Declaration);
struct Declaration : public Statement {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple(
			MEMBER_PAIR(*this, name)
		);
	}
	NON_COPY_FORMATTABLE(Declaration, Statement);
	Declaration(cat::String&& name, const Position& pos)
		: Statement(pos), name(std::move(name)) { };

	cat::String name;
};


PTRS_FOR_STRUCT(VarDeclaration);
struct VarDeclaration : public Declaration {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple(
			MEMBER_PAIR(*this, type),
			MEMBER_PAIR(*this, initExpr),
			MEMBER_PAIR(*this, isConst)
		);
	}
	NON_COPY_FORMATTABLE(VarDeclaration, Declaration);
	VarDeclaration(cat::String&& name, TypeExprPtr&& type, ExpressionPtr&& initExpr, bool isConst, const Position& pos)
		: Declaration(std::move(name), pos), type(std::move(type)), initExpr(std::move(initExpr)), isConst(isConst) { };

	TypeExprPtr type;
	ExpressionPtr initExpr;
	bool isConst;

};


PTRS_FOR_STRUCT(Assignment);
struct Assignment : public Statement {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple(
			MEMBER_PAIR(*this, variable),
			MEMBER_PAIR(*this, expr)
		);
	}
	NON_COPY_FORMATTABLE(Assignment, Statement);
	Assignment(VariableReferencePtr&& variable, ExpressionPtr&& expr, const Position& pos)
		: Statement(pos), variable(std::move(variable)), expr(std::move(expr)) { };

	VariableReferencePtr variable;
	ExpressionPtr expr;
};


PTRS_FOR_STRUCT(Block);
struct Block : public Node {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple(
			MEMBER_PAIR(*this, statements)
		);
	}
	NON_COPY_FORMATTABLE(Block, Node);
	std::vector<StatementPtr> statements;

	Block(std::vector<StatementPtr>&& statements, const Position& pos)
		: Node(pos), statements(std::move(statements)) 
	{}

};


PTRS_FOR_STRUCT(IfControl);
struct IfControl : public Statement {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple(
			MEMBER_PAIR(*this, condition),
			MEMBER_PAIR(*this, thenBlock),
			MEMBER_PAIR(*this, elseBlock)
		);
	}
	NON_COPY_FORMATTABLE(IfControl, Statement);
	ExpressionPtr condition;
	BlockPtr thenBlock;
	BlockPtr elseBlock;

	IfControl(ExpressionPtr&& condition, BlockPtr&& thenBlock, BlockPtr&& elseBlock, const Position& pos)
		: Statement(pos),
		  condition(std::move(condition)),
		  thenBlock(std::move(thenBlock)),
		  elseBlock(std::move(elseBlock)) {}
};


PTRS_FOR_STRUCT(WhileControl);
struct WhileControl : public Statement {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple(
			MEMBER_PAIR(*this, condition),
			MEMBER_PAIR(*this, block)
		);
	}
	NON_COPY_FORMATTABLE(WhileControl, Statement);
	WhileControl(ExpressionPtr&& condition, BlockPtr&& block, const Position& pos)
		: Statement(pos),
		  condition(std::move(condition)),
		  block(std::move(block))
	{}

	ExpressionPtr condition;
	BlockPtr block;
};


PTRS_FOR_STRUCT(ReturnStatement);
struct ReturnStatement : public Statement {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple(
			MEMBER_PAIR(*this, expr)
		);
	}
	NON_COPY_FORMATTABLE(ReturnStatement, Statement);
	ReturnStatement(ExpressionPtr&& expr, const Position& pos)
		: Statement(pos), expr(std::move(expr)) { };

	ExpressionPtr expr;
};


PTRS_FOR_STRUCT(ParamDeclaration);
struct ParamDeclaration : public Node {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple(
			MEMBER_PAIR(*this, name),
			MEMBER_PAIR(*this, type)
		);
	}
	NON_COPY_FORMATTABLE(ParamDeclaration, Node);
	ParamDeclaration(cat::String&& name, TypeExprPtr&& type, const Position& pos)
		: Node(pos), name(std::move(name)), type(std::move(type)) { };

	cat::String name;
	TypeExprPtr type;

};


PTRS_FOR_STRUCT(FuncDeclaration);
struct FuncDeclaration : public Declaration {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple(
			MEMBER_PAIR(*this, returnType),
			MEMBER_PAIR(*this, parameters),
			MEMBER_PAIR(*this, block)
		);
	}
	NON_COPY_FORMATTABLE(FuncDeclaration, Declaration);
	FuncDeclaration(cat::String&& name, TypeExprPtr&& returnType, std::vector<ParamDeclarationPtr>&& params, BlockPtr&& block, const Position& pos)
		: Declaration(std::move(name), pos),
		  returnType(std::move(returnType)),
		  parameters(std::move(params)),
		  block(std::move(block))
	{ };

	TypeExprPtr returnType;
	std::vector<ParamDeclarationPtr> parameters;
	BlockPtr block;
};

PTRS_FOR_STRUCT(CtorDeclaration);
struct CtorDeclaration : public Declaration {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple(
			MEMBER_PAIR(*this, parameters),
			MEMBER_PAIR(*this, block)
		);
	}
	NON_COPY_FORMATTABLE(CtorDeclaration, Declaration);
	CtorDeclaration(cat::String&& name, std::vector<ParamDeclarationPtr>&& params, BlockPtr&& block, const Position& pos)
		: Declaration(std::move(name), pos),
		  parameters(std::move(params)),
		  block(std::move(block))
	{ };

	std::vector<ParamDeclarationPtr> parameters;
	BlockPtr block;
};

PTRS_FOR_STRUCT(DtorDeclaration);
struct DtorDeclaration : public Declaration {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple(
			MEMBER_PAIR(*this, block)
		);
	}
	NON_COPY_FORMATTABLE(DtorDeclaration, Declaration);
	DtorDeclaration(BlockPtr&& block, const Position& pos)
		: Declaration("", pos),
		  block(std::move(block))
	{ };

	BlockPtr block;
};

PTRS_FOR_STRUCT(TypeDeclaration);
struct TypeDeclaration : public Declaration {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple(
			MEMBER_PAIR(*this, name)
		);
	}
	NON_COPY_FORMATTABLE(TypeDeclaration, Declaration);
	TypeDeclaration(cat::String&& name, std::vector<DeclarationPtr>&& members, const Position& pos)
		: Declaration(std::move(name), pos),
		  members(std::move(members))
	{ };

	std::vector<DeclarationPtr> members;
};


PTRS_FOR_STRUCT(StructDeclaration);
struct StructDeclaration : public TypeDeclaration {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple();
	}
	NON_COPY_FORMATTABLE(StructDeclaration, TypeDeclaration);
	StructDeclaration(cat::String&& name, std::vector<DeclarationPtr>&& members, const Position& pos)
		: TypeDeclaration(std::move(name), std::move(members), pos)
	{ };
};


PTRS_FOR_STRUCT(ClassDeclaration);
struct ClassDeclaration : public TypeDeclaration {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple();
	}
	NON_COPY_FORMATTABLE(ClassDeclaration, TypeDeclaration);
	ClassDeclaration(cat::String&& name, std::vector<DeclarationPtr>&& members, const Position& pos)
		: TypeDeclaration(std::move(name), std::move(members), pos)
	{ };

};


PTRS_FOR_STRUCT(UnitDeclaration);
struct UnitDeclaration : public Declaration {
	auto _getLocalMembersTuple() const {
		return cat::makeCRefTuple(
			MEMBER_PAIR(*this, unitNature),
			MEMBER_PAIR(*this, block)
		);
	}
	NON_COPY_FORMATTABLE(UnitDeclaration, Declaration);
	UnitDeclaration(cat::String&& name, UnitNature unitNature, BlockPtr&& block, const Position& pos)
		: Declaration(std::move(name), pos),
		  unitNature(unitNature),
		  block(std::move(block))
	{ };

	UnitNature unitNature;
	BlockPtr block;
};




}

#endif // NODE_H

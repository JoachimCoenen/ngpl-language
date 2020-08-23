#ifndef NODE_H
#define NODE_H

#include "position.h"
#include "unitNature.h"

#include "util/types.h"

#include "cat_typing.h"
#include "toStringUtils.h"
#include "ranges.h"

#include <string>
#include <vector>

namespace ngpl {

struct IFormattable {
	virtual std::string getTypeName() const { return "IFormattable";}
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
	std::string getTypeName() const override {	\
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

STRUCT_WITH_PTR(Node) : public ngpl::IFormattable {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple();
	}
	NON_COPY_FORMATTABLE(Node, ngpl::IFormattable)


	Node(const Position pos)
		: ngpl::IFormattable(), pos(pos)
	{};
	virtual ~Node() {}

	Position pos;

};


STRUCT_WITH_PTR(Statement) : public Node {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple();
	}
	NON_COPY_FORMATTABLE(Statement, Node)
	Statement(const Position& pos) 
		: Node(pos) 
	{ };

};


STRUCT_WITH_PTR(Root) : public Node {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple(
			MEMBER_PAIR(*this, statements)
		);
	}
	NON_COPY_FORMATTABLE(Root, Node)
	std::vector<StatementPtr> statements;

	Root(const Position& pos) : Node(pos), statements() {}
	Root(std::vector<StatementPtr>&& statements, const Position& pos)
		: Node(pos), statements(std::move(statements)) 
	{}

};


STRUCT_WITH_PTR(Expression) : public Statement {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple();
	}
	NON_COPY_FORMATTABLE(Expression, Statement)
	Expression(const Position& pos) 
		: Statement(pos)
	{ };
};


STRUCT_WITH_PTR(Literal) : public Expression {
	NON_COPY_FORMATTABLE(Literal, Expression)

	Literal(const Position& pos) 
		: Expression(pos) 
	{}

	// auto get() const = 0;
};


STRUCT_WITH_PTR(LiteralBool) : public Literal {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple(
			MEMBER_PAIR(*this, value)
		);
	}
	NON_COPY_FORMATTABLE(LiteralBool, Literal)
	bool value;

	LiteralBool(bool v, const Position& pos)
		: Literal(pos), value(v)
	{}

	auto get() const {
		return value;
	}
};


STRUCT_WITH_PTR(LiteralInt) : public Literal {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple(
			MEMBER_PAIR(*this, value)
		);
	}
	NON_COPY_FORMATTABLE(LiteralInt, Literal)
	int64_t value;

	LiteralInt(int64_t v, const Position& pos)
		: Literal(pos), value(v)
	{}

	auto get() const {
		return value;
	}
};


STRUCT_WITH_PTR(LiteralString) : public Literal {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple(
			MEMBER_PAIR(*this, value)
		);
	}
	NON_COPY_FORMATTABLE(LiteralString, Literal)
	std::string value;

	LiteralString(const std::string& v, const Position& pos) 
		: Literal(pos), value(v) 
	{}
	LiteralString(std::string&& v, const Position& pos) 
		: Literal(pos), value(std::move(v)) 
	{}

	const auto& get() const {
		return value;
	}
};


STRUCT_WITH_PTR(VariableReference) : public Expression {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple(
			MEMBER_PAIR(*this, name)
		);
	}
	NON_COPY_FORMATTABLE(VariableReference, Expression)
	std::string name;

	VariableReference(std::string&& name, const Position& pos)
	   : Expression(pos), name(std::move(name)) {}
};


STRUCT_WITH_PTR(MemberAccess) : public VariableReference {
   auto _getLocalMembersTuple() const {
	   return cat::makeCTuple(
		   // MEMBER_PAIR(*this, name)
	   );
   }
   NON_COPY_FORMATTABLE(MemberAccess, VariableReference)
	ExpressionPtr parent;

	MemberAccess(std::string&& name, ExpressionPtr&& parent, const Position& pos)
	  : VariableReference(std::move(name), pos), parent(std::move(parent)) {}
};


STRUCT_WITH_PTR(FunctionCall) : public Expression {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple(
			MEMBER_PAIR(*this, name),
			MEMBER_PAIR(*this, parent),
			MEMBER_PAIR(*this, arguments)
		);
	}
	NON_COPY_FORMATTABLE(FunctionCall, Expression)
	std::string name;
	ExpressionPtr parent;
	std::vector<ExpressionPtr> arguments;

	FunctionCall(std::string&& name, ExpressionPtr&& parent, std::vector<ExpressionPtr>&& args, const Position& pos)
	   : Expression(pos), name(std::move(name)), parent(std::move(parent)), arguments(std::move(args)) {}
};


STRUCT_WITH_PTR(UnaryOperatorCall) : public Expression {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple(
			MEMBER_PAIR(*this, name),
			MEMBER_PAIR(*this, operand)
		);
	}
	NON_COPY_FORMATTABLE(UnaryOperatorCall, Expression)
	std::string name;
	ExpressionPtr operand;

	UnaryOperatorCall(std::string&& name, ExpressionPtr&& operand, const Position& pos)
		: Expression(pos), name(std::move(name)), operand(std::move(operand)) {}
};


STRUCT_WITH_PTR(BinaryOperatorCall) : public Expression {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple(
			MEMBER_PAIR(*this, name),
			MEMBER_PAIR(*this, lhs),
			MEMBER_PAIR(*this, rhs)
		);
	}
	NON_COPY_FORMATTABLE(BinaryOperatorCall, Expression)
	std::string name;
	ExpressionPtr lhs;
	ExpressionPtr rhs;

	BinaryOperatorCall(std::string&& name, ExpressionPtr&& lhs, ExpressionPtr&& rhs, const Position& pos)
		: Expression(pos), name(std::move(name)), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

	static int getPrecedence(const std::string& name);
	int getPrecedence() const { return getPrecedence(name); }
};


STRUCT_WITH_PTR(TypeExpr) : public Node {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple(
			MEMBER_PAIR(*this, name)
		);
	}
	NON_COPY_FORMATTABLE(TypeExpr, Node)
	TypeExpr(std::string&& name, const Position& pos)
		: Node(pos), name(std::move(name)) { };

	std::string name;
};


STRUCT_WITH_PTR(Declaration) : public Statement {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple(
			MEMBER_PAIR(*this, name)
		);
	}
	NON_COPY_FORMATTABLE(Declaration, Statement)
	Declaration(std::string&& name, const Position& pos)
		: Statement(pos), name(std::move(name)) { };

	std::string name;
};


STRUCT_WITH_PTR(VarDeclaration) : public Declaration {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple(
			MEMBER_PAIR(*this, type),
			MEMBER_PAIR(*this, initExpr)
		);
	}
	NON_COPY_FORMATTABLE(VarDeclaration, Declaration)
	VarDeclaration(std::string&& name, TypeExprPtr&& type, ExpressionPtr&& initExpr, const Position& pos)
		: Declaration(std::move(name), pos), type(std::move(type)), initExpr(std::move(initExpr)) { };

	TypeExprPtr type;
	ExpressionPtr initExpr;

};


STRUCT_WITH_PTR(ConstDeclaration) : public VarDeclaration {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple();
	}
	NON_COPY_FORMATTABLE(ConstDeclaration, VarDeclaration)
	ConstDeclaration(std::string&& name, TypeExprPtr&& type, ExpressionPtr&& initExpr, const Position& pos)
		: VarDeclaration(std::move(name), std::move(type), std::move(initExpr), pos) { };
};


STRUCT_WITH_PTR(Assignment) : public Statement {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple(
			MEMBER_PAIR(*this, variable),
			MEMBER_PAIR(*this, expr)
		);
	}
	NON_COPY_FORMATTABLE(Assignment, Statement)
	Assignment(VariableReferencePtr&& variable, ExpressionPtr&& expr, const Position& pos)
		: Statement(pos), variable(std::move(variable)), expr(std::move(expr)) { };

	VariableReferencePtr variable;
	ExpressionPtr expr;
};


STRUCT_WITH_PTR(Block) : public Node {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple(
			MEMBER_PAIR(*this, statements)
		);
	}
	NON_COPY_FORMATTABLE(Block, Node)
	std::vector<StatementPtr> statements;

	Block(std::vector<StatementPtr>&& statements, const Position& pos)
		: Node(pos), statements(std::move(statements)) 
	{}

};


STRUCT_WITH_PTR(IfControl) : public Statement {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple(
			MEMBER_PAIR(*this, condition),
			MEMBER_PAIR(*this, thenBlock),
			MEMBER_PAIR(*this, elseBlock)
		);
	}
	NON_COPY_FORMATTABLE(IfControl, Statement)
	ExpressionPtr condition;
	BlockPtr thenBlock;
	BlockPtr elseBlock;

	IfControl(ExpressionPtr&& condition, BlockPtr&& thenBlock, BlockPtr&& elseBlock, const Position& pos)
		: Statement(pos),
		  condition(std::move(condition)),
		  thenBlock(std::move(thenBlock)),
		  elseBlock(std::move(elseBlock)) {}
};


STRUCT_WITH_PTR(WhileControl) : public Statement {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple(
			MEMBER_PAIR(*this, condition),
			MEMBER_PAIR(*this, block)
		);
	}
	NON_COPY_FORMATTABLE(WhileControl, Statement)
	WhileControl(ExpressionPtr&& condition, BlockPtr&& block, const Position& pos)
		: Statement(pos),
		  condition(std::move(condition)),
		  block(std::move(block))
	{}

	ExpressionPtr condition;
	BlockPtr block;
};


STRUCT_WITH_PTR(ReturnStatement) : public Statement {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple(
			MEMBER_PAIR(*this, expr)
		);
	}
	NON_COPY_FORMATTABLE(ReturnStatement, Statement)
	ReturnStatement(ExpressionPtr&& expr, const Position& pos)
		: Statement(pos), expr(std::move(expr)) { };

	ExpressionPtr expr;
};


STRUCT_WITH_PTR(ParamDeclaration) : public Node {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple(
			MEMBER_PAIR(*this, name),
			MEMBER_PAIR(*this, type)
		);
	}
	NON_COPY_FORMATTABLE(ParamDeclaration, Node)
	ParamDeclaration(std::string&& name, TypeExprPtr&& type, const Position& pos)
		: Node(pos), name(std::move(name)), type(std::move(type)) { };

	std::string name;
	TypeExprPtr type;

};


STRUCT_WITH_PTR(FuncDeclaration) : public Declaration {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple(
			MEMBER_PAIR(*this, returnType),
			MEMBER_PAIR(*this, parameters),
			MEMBER_PAIR(*this, block)
		);
	}
	NON_COPY_FORMATTABLE(FuncDeclaration, Declaration)
	FuncDeclaration(std::string&& name, TypeExprPtr&& returnType, std::vector<ParamDeclarationPtr>&& params, BlockPtr&& block, const Position& pos)
		: Declaration(std::move(name), pos),
		  returnType(std::move(returnType)),
		  parameters(std::move(params)),
		  block(std::move(block))
	{ };

	TypeExprPtr returnType;
	std::vector<ParamDeclarationPtr> parameters;
	BlockPtr block;
};


STRUCT_WITH_PTR(TypeDeclaration) : public Declaration {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple();
	}
	NON_COPY_FORMATTABLE(TypeDeclaration, Declaration)
	TypeDeclaration(std::string&& name, const Position& pos)
		: Declaration(std::move(name), pos)
	{ };
};


STRUCT_WITH_PTR(StructDeclaration) : public TypeDeclaration {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple(
			MEMBER_PAIR(*this, name)
		);
	}
	NON_COPY_FORMATTABLE(StructDeclaration, TypeDeclaration)
	StructDeclaration(std::string&& name, std::vector<DeclarationPtr>&& members, const Position& pos)
		: TypeDeclaration(std::move(name), pos),
		  members(std::move(members))
	{ };

	std::vector<DeclarationPtr> members;
};


STRUCT_WITH_PTR(UnitDeclaration) : public Declaration {
	auto _getLocalMembersTuple() const {
		return cat::makeCTuple(
			MEMBER_PAIR(*this, unitNature),
			MEMBER_PAIR(*this, block)
		);
	}
	NON_COPY_FORMATTABLE(UnitDeclaration, Declaration)
	UnitDeclaration(std::string&& name, UnitNature unitNature, BlockPtr&& block, const Position& pos)
		: Declaration(std::move(name), pos),
		  unitNature(unitNature),
		  block(std::move(block))
	{ };

	UnitNature unitNature;
	BlockPtr block;
};




}

#endif // NODE_H

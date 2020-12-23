#include "parser.h"

#include "util/debug.h"

#include "syntaxError.h"

#include "cat_utils.h"
#include "cat_stack.h"


using cat::SW;


namespace ngpl { // Parser

Parser::Parser(Tokenizer&& tokenizer) : tokenizer(std::move(tokenizer))
{

}

RootPtr Parser::parseRoot()
{
	auto pos = tokenizer.getPos();
	if (wouldAcceptAnyOfToken({"unit",  "program", "package", "library"})) {
		std::vector<StatementPtr> statements;
		statements.push_back(parseUnit());
		return {new Root{std::move(statements), pos}};
	} else {
		auto block = parseBlock({});
		return {new Root{std::move(block->statements), pos}};
	}
}

UnitDeclarationPtr Parser::parseUnit()
{
	Token tk = acceptAnyOfToken({"unit",  "program", "package", "library"});
	UnitNature unitNature;

	if (false) {
		// NOP();
	} else if (tk.content == "unit") {
		unitNature = UnitNature::UNIT;
	} else if (tk.content == "program") {
		unitNature = UnitNature::PROGRAM;
	} else if (tk.content == "package") {
		unitNature = UnitNature::PACKAGE;
	} else if (tk.content == "library") {
		unitNature = UnitNature::LIBRARY;
	} else {
		NGPL_ASSERT(false);
		unitNature = UnitNature::LIBRARY; // make ther warnings happy...
	}

	auto name = acceptToken(TokenKind::IDENTIFIER).content;
	auto block = parseBlock({});
	return {new UnitDeclaration{std::move(name), unitNature, std::move(block), tk.pos}};
}

StatementPtr Parser::parseStatement()
{
	if (wouldAcceptToken("if")) {
		return parseIfControl();
	}
	if (wouldAcceptToken("while")) {
		return parseWhileControl();
	}
	if (wouldAcceptToken("return")) {
		return parseReturnStatement();
	}

	if (wouldAcceptToken(TokenKind::IDENTIFIER)) {
		tokenizer.startSimulation();
		VariableReferencePtr lexpr = nullptr;
		try {
			lexpr = parseExpressionNoOp().asOwning<VariableReference>();
		} catch (SyntaxError&) {
			lexpr = nullptr;
		}

		if (lexpr and wouldAcceptToken("=")) {
			tokenizer.acceptSimulation();
			return  parseAssignment(std::move(lexpr));

		} else {
			tokenizer.discardSimulation();
			return parseExpression();
		}
	}

	auto declaration = tryParseDeclaration();
	if (declaration) {
		return declaration;
	}

	return parseExpression();
}

DeclarationPtr Parser::tryParseDeclaration()
{
	if (wouldAcceptToken("let")) {
		return parseConstDeclaration();
	}
	if (wouldAcceptToken("var")) {
		return parseVarDeclaration();
	}
	if (wouldAcceptToken("func")) {
		return parseFuncDeclaration();
	}
	if (wouldAcceptToken("type")) {
		return parseTypeDeclaration();
	}

	return nullptr;
}

DeclarationPtr Parser::parseDeclaration()
{
	auto declaration = tryParseDeclaration();
	if (not declaration) {
		throw SyntaxError(cat::SW() << "Expected a declaration but found '" << currentToken().content << "'.", currentToken().pos);
	}
	return declaration;
}

FuncDeclarationPtr Parser::parseFuncDeclaration()
{
	auto pos = acceptToken("func").pos;
	auto name = acceptToken(TokenKind::IDENTIFIER).content;

	auto parameters = parseParameters();

	TypeExprPtr returnType = nullptr;
	if (tryAcceptToken("->")) {
	returnType = parseTypeExpr();
	}
	acceptToken("{");
	auto block = parseBlock({"}"});
	acceptToken("}");

	return {new FuncDeclaration{std::move(name), std::move(returnType), std::move(parameters), std::move(block), pos}};
}

std::vector<ParamDeclarationPtr> Parser::parseParameters()
{
	std::vector<ParamDeclarationPtr> result;
	acceptToken("(");
	if (wouldAcceptToken(")")) {
	acceptToken(")");
	return result;
	}


	result.push_back(parseParameter());
	while (tryAcceptToken(",")) {
	result.push_back(parseParameter());
	}
	acceptToken(")");

	return result;
}

ParamDeclarationPtr Parser::parseParameter()
{
	auto nameTK = acceptToken(TokenKind::IDENTIFIER);
	acceptToken(":");
	auto type = parseTypeExpr();

	return {new ParamDeclaration{std::move(nameTK.content), std::move(type), nameTK.pos}};

}

ConstDeclarationPtr Parser::parseConstDeclaration()
{
	auto pos = acceptToken("let").pos;

	auto name = acceptToken(TokenKind::IDENTIFIER);

	TypeExprPtr type = nullptr;
	if (tryAcceptToken(":")) {
	type = parseTypeExpr();
	}

	ExpressionPtr initExpr = nullptr;
	if (tryAcceptToken("=")) {
		initExpr = parseExpression();
	} else {
		throw SyntaxError(cat::SW() << "Missing initialisatipn of constant '" << name.content << "'.", name.pos);
	}

	return {new ConstDeclaration{std::move(name.content), std::move(type), std::move(initExpr), pos}};
}

VarDeclarationPtr Parser::parseVarDeclaration()
{
	auto pos = acceptToken("var").pos;
	auto nameTk = acceptToken(TokenKind::IDENTIFIER);

	TypeExprPtr type = nullptr;
	if (tryAcceptToken(":")) {
		type = parseTypeExpr();
	}

	ExpressionPtr initExpr = nullptr;
	if (tryAcceptToken("=")) {
		initExpr = parseExpression();
	}

	return {new VarDeclaration{std::move(nameTk.content), std::move(type), std::move(initExpr), pos}};
}

TypeDeclarationPtr Parser::parseTypeDeclaration()
{
	auto pos = acceptToken("type").pos;
	auto nameTk = acceptToken(TokenKind::IDENTIFIER);
	acceptToken("=");

	if (tryAcceptToken("struct")) {
		acceptToken("{");

		std::vector<DeclarationPtr> declarations;
		while (not tokenizer.isPastEnd() and not cat::isAnyOf(currentToken().content, "}")) {
			declarations.push_back(parseDeclaration());
		}
		acceptToken("}");

		return { new StructDeclaration(std::move(nameTk.content), std::move(declarations), pos)};

	} else {
		throw SyntaxError(cat::SW() << "Unsupprted Type declaration for 'type " << nameTk.content << "' = " << currentToken().content << "'.", currentToken().pos);
	}


}

TypeExprPtr Parser::parseTypeExpr()
{
	auto name = acceptToken(TokenKind::IDENTIFIER);
	return {new TypeExpr{std::move(name.content), name.pos}};
}

AssignmentPtr Parser::parseAssignment(VariableReferencePtr&& variableReference)
{
	auto pos = acceptToken("=").pos;
	auto expr = parseExpression();
	return {new Assignment{std::move(variableReference), std::move(expr), pos}};
}

IfControlPtr Parser::parseIfControl()
{
	auto pos = acceptToken("if").pos;
	auto condition = parseExpression();
	acceptToken("do");
	acceptToken("{");
	auto thenBlock = parseBlock({"}", "else"});

	BlockPtr elseBlock = nullptr;
	if (tryAcceptToken("else")) {
	elseBlock = parseBlock({"}"});
	}
	acceptToken("}");
	return {new IfControl{std::move(condition), std::move(thenBlock), std::move(elseBlock), pos}};
}

WhileControlPtr Parser::parseWhileControl()
{
	auto pos = acceptToken("while").pos;
	auto condition = parseExpression();
	acceptToken("do");
	acceptToken("{");
	auto block = parseBlock({"}"});
	acceptToken("}");
	return {new WhileControl{std::move(condition), std::move(block), pos}};
}

ReturnStatementPtr Parser::parseReturnStatement()
{
	auto pos = acceptToken("return").pos;
	auto expr = parseExpression();
	return {new ReturnStatement{std::move(expr), pos}};

}

ExpressionPtr Parser::parseExpressionNoOp()
{
	ExpressionPtr expr = nullptr;
	const auto& token = tokenizer.get();
	switch (token.kind) {
	case TokenKind::NUMBER:
	expr = parseInteger();
	break;
	case TokenKind::STRING:
	expr = parseString();
	break;
	case TokenKind::BOOLEAN:
	expr = parseBoolean();
	break;
	case TokenKind::IDENTIFIER:
	expr = parseVarReferenceOrFunctionCall();
	break;
	case TokenKind::OPERATOR:
	expr = parseUnaryOperator();
	break;
	case TokenKind::SEPARATOR:

	acceptToken("(");
	expr = parseExpression();
	acceptToken(")");
	break;
	default:
	throw SyntaxError(cat::SW() << "Invalid token of type " << token.kind << ".", token.pos);
	}

	while(tryAcceptToken(".")) {
		auto identifierTk = acceptToken(TokenKind::IDENTIFIER);


		if (wouldAcceptToken("(")) {
			auto arguments = parseTuple();
			expr =  {new FunctionCall{std::move(identifierTk.content), std::move(expr), std::move(arguments), identifierTk.pos}};
		} else {
			expr = MemberAccessPtr({}, std::move(identifierTk.content), std::move(expr), identifierTk.pos);
		}
	}

	return expr;
}

ExpressionPtr Parser::parseExpression()
{
	cat::Stack<ExpressionPtr> exprStack;
	cat::Stack<Token> operStack;
	exprStack.push(parseExpressionNoOp());

	while (wouldAcceptToken(TokenKind::OPERATOR)) {
		operStack.push(acceptToken(TokenKind::OPERATOR));
		exprStack.push(parseExpressionNoOp());
	}

	cat::Stack<ExpressionPtr> exprParking;
	cat::Stack<Token> operParking;
	while (operStack.size() >= 1) {
		if (operParking.size() < 1) {
			exprParking.push(exprStack.pop());
			operParking.push(operStack.pop());
			continue;
		}
		const auto ropPrecedence = BinaryOperatorCall::getPrecedence(operParking.peek().content);
		const auto lopPrecedence = BinaryOperatorCall::getPrecedence(operStack.peek().content);
		if (lopPrecedence >= ropPrecedence) {
			exprParking.push(exprStack.pop());
			operParking.push(operStack.pop());
		} else {
			auto rhs = exprParking.pop();
			auto lhs = exprStack.pop();

			auto rop = operParking.pop();
			exprStack.push(BinaryOperatorCallPtr({},  std::move(rop.content), std::move(lhs), std::move(rhs), rop.pos));
		}
	}

	if (exprStack.size() != 1) {
		throw cat::SimpleError(cat::SW() << "Invalid expressions stack size while parsing an expression.");
	}

	// at last, collapse all parked operators:
	auto expr = exprStack.pop();
	while (exprParking.size() >= 1) {
		auto rhs = exprParking.pop();
		auto op = operParking.pop();
		expr = BinaryOperatorCallPtr({},  std::move(op.content), std::move(expr), std::move(rhs), op.pos);
	}

	if (exprParking.size() != 0) {
		throw cat::SimpleError(cat::SW() << "Invalid expressions parking stack size while parsing an expression.");
	}
	if (operParking.size() != 0) {
		throw cat::SimpleError(cat::SW() << "Invalid operators parking stack size while parsing an expression.");
	}

	return expr;
	//Expression
}

BlockPtr Parser::parseBlock(const std::initializer_list<std::string>& endMarkers)
{
	auto pos = currentToken().pos;
	std::vector<StatementPtr> statements;
	while (not tokenizer.isPastEnd() and not cat::isAnyOf_alt(currentToken().content, endMarkers)) {
		statements.push_back(parseStatement());
	}
	return {new Block{std::move(statements), pos}};
}

ExpressionPtr Parser::parseVarReferenceOrFunctionCall()
{
	auto identifier = acceptToken(TokenKind::IDENTIFIER);
	if (wouldAcceptToken("(")) {
		auto arguments = parseTuple();
		return {new FunctionCall{ std::move(identifier.content), nullptr, std::move(arguments), identifier.pos}};
	} else {
		return {new VariableReference{ std::move(identifier.content), identifier.pos}};
	}


}

UnaryOperatorCallPtr Parser::parseUnaryOperator()
{
	auto oper = acceptToken(TokenKind::OPERATOR);
	if (not cat::isAnyOf(oper.content, "-", "+", "!")) {
		throw SyntaxError(cat::SW() << "Expected a unary operator, but found '" << oper.content << "' (which is a binary operator).", oper.pos);
	}
	auto operand = parseExpression();
	return {new UnaryOperatorCall{std::move(oper.content), std::move(operand), oper.pos}};
}

std::vector<ExpressionPtr> Parser::parseTuple()
{
	std::vector<ExpressionPtr> result;
	acceptToken("(");
	if (wouldAcceptToken(")")) {
		acceptToken(")");
		return result;
	}


	result.push_back(parseExpression());
	while (tryAcceptToken(",")) {
		result.push_back(parseExpression());
	}
	acceptToken(")");

	return result;

}

LiteralBoolPtr Parser::parseBoolean()
{
	const auto token = acceptToken(TokenKind::BOOLEAN);
	return {new LiteralBool{ token.content == "true", token.pos}};
}

LiteralIntPtr Parser::parseInteger()
{
	const auto token = acceptToken(TokenKind::NUMBER);
	return {new LiteralInt{ std::stoi(token.content), token.pos}};
}

LiteralStringPtr Parser::parseString()
{
	const auto token = acceptToken(TokenKind::STRING);
	return {new LiteralString{ token.content, token.pos}};
}

}


namespace ngpl { // Acceptors:

bool Parser::wouldAcceptToken(TokenKind kind) const
{
	return currentToken().kind == kind;
}

bool Parser::wouldAcceptToken(const std::string& str) const
{
	return currentToken().content == str;
}

bool Parser::wouldAcceptAnyOfToken(const std::initializer_list<std::string>& strs) const
{
	return cat::isAnyOf_alt(currentToken().content, strs);
}

bool Parser::tryAcceptToken(TokenKind kind)
{
	if (wouldAcceptToken(kind)) {
		checkNotEndOfStream(kind); // throws
		tokenizer.advance();
		return true;
	}
	return false;
}

bool Parser::tryAcceptToken(const std::string& str)
{
	if (wouldAcceptToken(str)) {
		checkNotEndOfStream(str); // throws
		tokenizer.advance();
		return true;
	}
	return false;
}

bool Parser::tryAcceptAnyOfToken(const std::initializer_list<std::string>& strs)
{
	if (wouldAcceptAnyOfToken(strs)) {
//		std::string expectation = cat::SW()
//				<< "any of: "
//				<< cat::range(strs).map(LAMBDA(s){ return cat::formatVal(s); }).join(", ");
//		checkNotEndOfStreamNonFormatted(expectation); // throws
		auto strsv = std::vector(strs);
		auto sep = std::string(", ");
		checkNotEndOfStream(cat::range(strsv).join(sep)); // throws
		tokenizer.advance();
		return true;
	}
	return false;
}

Token Parser::acceptToken(TokenKind kind)
{
	checkNotEndOfStream(kind); // throws
	auto token = currentToken();
	if (not wouldAcceptToken(kind)) {
		throw SyntaxError(cat::SW() << "Expected " << kind << ", but got " << token.kind << " "<< cat::formatVal(token.content) << ".", token.pos);
	}

	tokenizer.advance();
	return token;
}

Token Parser::acceptToken(const std::string& str)
{
	checkNotEndOfStream(str);
	auto token = currentToken();
	if (not wouldAcceptToken(str)) {
		auto token = currentToken();
		auto sE = SyntaxError(cat::SW() << "Expected " << cat::formatVal(str) << ", but got " << cat::formatVal(token.content) << ".", token.pos);
		throw sE;
	}

	tokenizer.advance();
	return token;
}

Token Parser::acceptAnyOfToken(const std::initializer_list<std::string>& strs)
{
	auto strsv = std::vector(strs);
	auto sep = std::string(", ");
	checkNotEndOfStream(cat::range(strsv).join(sep));
	auto token = currentToken();
	if (not wouldAcceptAnyOfToken(strs)) {
		auto token = currentToken();
		auto sE = SyntaxError(cat::SW() << "Expected " << cat::range(strsv).join(sep) << ", but got " << cat::formatVal(token.content) << ".", token.pos);
		throw sE;
	}

	tokenizer.advance();
	return token;
}

}


namespace ngpl { // Errors

void Parser::raiseEndOfStreamError(const std::string& expectation) const
{
	throw SyntaxError(cat::SW() << "Reached end fostream, but expected " << expectation << ".", tokenizer.getPos());
}
}

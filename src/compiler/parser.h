#ifndef PARSER_H
#define PARSER_H

#include "tokenizer.h"
#include "../language/ast.h"
#include "../language/token.h"

#include <string>

namespace ngpl {



class Parser
{
public:
	Parser(Tokenizer&& tokenizer);

	RootPtr parseRoot();

	UnitDeclarationPtr parseUnit();

	StatementPtr parseStatement();
	DeclarationPtr tryParseDeclaration();
	DeclarationPtr parseDeclaration();
	FuncDeclarationPtr parseFuncDeclaration();
	OwningPtrVector<ParamDeclaration> parseParameters();
	ParamDeclarationPtr parseParameter();
	ConstDeclarationPtr parseConstDeclaration();
	VarDeclarationPtr parseVarDeclaration();
	TypeDeclarationPtr parseTypeDeclaration();

	AssignmentPtr parseAssignment(VariableReferencePtr&& variableReference);
	IfControlPtr parseIfControl();
	WhileControlPtr parseWhileControl();
	ReturnStatementPtr parseReturnStatement();


	TypeExprPtr parseTypeExpr();
	ExpressionPtr parseExpressionNoOp();
	ExpressionPtr parseExpression();

	BlockPtr parseBlock(const std::initializer_list<std::string>& endMarkers);
	ExpressionPtr parseVarReferenceOrFunctionCall();
	UnaryOperatorCallPtr parseUnaryOperator();

	OwningPtrVector<Expression> parseTuple();
	LiteralBoolPtr parseBoolean();
	LiteralIntPtr parseInteger();
	LiteralStringPtr parseString();


protected:
	LookAheadIterator tokenizer;

	bool wouldAcceptToken(TokenKind kind) const;
	bool wouldAcceptToken(const std::string& str) const;
	bool wouldAcceptAnyOfToken(const std::initializer_list<std::string>& strs) const;

	const Token& currentToken() const { return tokenizer.get(); }

	bool tryAcceptToken(TokenKind kind);
	bool tryAcceptToken(const std::string& str);
	bool tryAcceptAnyOfToken(const std::initializer_list<std::string>& strs);

	Token acceptToken(TokenKind kind);
	Token acceptToken(const std::string& str);
	Token acceptAnyOfToken(const std::initializer_list<std::string>& strs);
	// TODO: Token acceptToken(std::initializer_list<std::string> strs);

protected:
	/**
	 * @brief raiseEndOfStreamError
	 * @param expectation
	 * @throws SyntaxError if end of stream is reached (tokenizer.isPastEnd() returns true)
	 */
	void raiseEndOfStreamError(const std::string& expectation) const;

	/**
	 * @brief checkNotEndOfStream
	 * @param expectation
	 * @throws SyntaxError if end of stream is reached (tokenizer.isPastEnd() returns true)
	 */
	template<class T>
	inline void checkNotEndOfStream(const T& expectation) const { // TODO: find better name.
		if (tokenizer.isPastEnd()) {
			raiseEndOfStreamError(cat::formatVal(expectation));
		}
	}

	/**
	 * @brief checkNotEndOfStream
	 * @param expectation
	 * @throws SyntaxError if end of stream is reached (tokenizer.isPastEnd() returns true)
	 */
	template<class T>
	inline void checkNotEndOfStreamNonFormatted(const T& expectation) const { // TODO: find better name.
		if (tokenizer.isPastEnd()) {
			raiseEndOfStreamError(expectation);
		}
	}
};

}

#endif // PARSER_H

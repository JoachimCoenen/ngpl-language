#ifndef PARSER_H
#define PARSER_H

#include "tokenizer.h"
#include "../language/ast.h"
#include "../language/token.h"

#include "cat_string.h"

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
	CtorDeclarationPtr parseCtorDeclaration();
	DtorDeclarationPtr parseDtorDeclaration();
	std::vector<ParamDeclarationPtr> parseParameters();
	ParamDeclarationPtr parseParameter();
	VarDeclarationPtr parseVarDeclaration();
	TypeDeclarationPtr parseTypeDeclaration();
	std::vector<DeclarationPtr> parseTypeBody();

	AssignmentPtr parseAssignment(VariableReferencePtr&& variableReference);
	IfControlPtr parseIfControl();
	WhileControlPtr parseWhileControl();
	ReturnStatementPtr parseReturnStatement();


	TypeExprPtr parseTypeExpr();
	ExpressionPtr parseExpressionNoOp();
	ExpressionPtr parseExpression();

	BlockPtr parseBlock(const std::initializer_list<cat::String>& endMarkers);
	ExpressionPtr parseVarReferenceOrFunctionCall();
	UnaryOperatorCallPtr parseUnaryOperator();

	std::vector<TypeExprPtr> parseTypeArguments();
	std::vector<ExpressionPtr> parseTuple();
	LiteralBoolPtr parseBoolean();
	LiteralNilPtr parseNil();
	LiteralIntPtr parseInteger();
	LiteralStringPtr parseString();


protected:
	LookAheadIterator tokenizer;

	bool wouldAcceptToken(TokenKind kind) const;
	bool wouldAcceptToken(const cat::String& str) const;
	bool wouldAcceptAnyOfToken(const std::initializer_list<cat::String>& strs) const;

	const Token& currentToken() const { return tokenizer.get(); }

	std::optional<Token> tryAcceptToken(TokenKind kind);
	bool tryAcceptToken(const cat::String& str);
	bool tryAcceptAnyOfToken(const std::initializer_list<cat::String>& strs);

	Token acceptToken(TokenKind kind);
	Token acceptToken(const cat::String& str);
	Token acceptAnyOfToken(const std::initializer_list<cat::String>& strs);
	// TODO: Token acceptToken(std::initializer_list<cat::String> strs);

protected:
	/**
	 * @brief raiseEndOfStreamError
	 * @param expectation
	 * @throws SyntaxError if end of stream is reached (tokenizer.isPastEnd() returns true)
	 */
	void raiseEndOfStreamError(const cat::String& expectation) const;

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

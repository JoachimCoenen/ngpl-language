#ifndef TOKEN_H
#define TOKEN_H

#include "position.h"
#include "toStringUtils.h"

#include <string>
#include <stdint.h>

namespace ngpl {

enum class  TokenKind {
    KEYWORD,
    IDENTIFIER,
    OPERATOR,
    SEPARATOR,
    NUMBER,
    STRING,
    BOOLEAN,
    COMMENT,

};
cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const TokenKind& v);

class Token {
public:
	Token() {}
	Token(TokenKind kind, const std::string& content, const Position& pos)
	: kind(kind), content(content), pos(pos) {}

	Token(TokenKind kind, std::string &&content, const Position& pos)
	: kind(kind), content(std::move(content)), pos(pos) {}

	Token(TokenKind kind, const std::string& content, Position &&pos)
	: kind(kind), content(content), pos(std::move(pos)) {}

	Token(TokenKind kind, std::string &&content, Position &&pos)
	: kind(kind), content(std::move(content)),  pos(std::move(pos)) {}

	TokenKind kind;
	std::string content;
	Position pos;

	bool operator == (const Token& other) const {
	return (kind == other.kind) and (content == other.content) and (pos == other.pos);
	}

};
cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const Token& v);


}


#endif // TOKEN_H

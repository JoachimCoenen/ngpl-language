#ifndef TOKEN_H
#define TOKEN_H

#include "position.h"

#include "cat_string.h"
#include "toStringUtils.h"


namespace ngpl {

enum class TokenKind {
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
	Token(TokenKind kind, const cat::String& content, const Position& pos)
	: kind(kind), content(content), pos(pos) {}

	Token(TokenKind kind, cat::String &&content, const Position& pos)
	: kind(kind), content(std::move(content)), pos(pos) {}

	Token(TokenKind kind, const cat::String& content, Position &&pos)
	: kind(kind), content(content), pos(std::move(pos)) {}

	Token(TokenKind kind, cat::String &&content, Position &&pos)
	: kind(kind), content(std::move(content)),  pos(std::move(pos)) {}

	TokenKind kind;
	cat::String content;
	Position pos;

	bool operator == (const Token& other) const {
	return (kind == other.kind) and (content == other.content) and (pos == other.pos);
	}

};
cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const Token& v);


}


#endif // TOKEN_H

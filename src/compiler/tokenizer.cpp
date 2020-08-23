#include "tokenizer.h"

#include "syntaxError.h"

#include "cat_utils.h"
#include "cat_typing.h"

#include "../util/debug.h"

namespace ngpl {  // Lexing

void Tokenizer::advance()
{
	skipWhitespacesAndCommentsIfAny();

	if (this->pos.index() == length()) {
		this->pos.addvanceChar();
		return;
	}

	char c = getChar();

	if (isIdentifierOrKeywordOpeningChar(c)) {
		readIdentifierOrKeyword();
	} else if (isDecimal(c)) {
		readNumber();
	} else if (isSeparator(c)) {
		readSeparator();
	} else if (isOperator(c)) {
		readOperator();
	} else if (c == '"') {
		readString();
	} else {
		throw SyntaxError(cat::SW() << "Stray '" << std::string()+getChar() << "' im program.", pos.get());
	}
}

void Tokenizer::readIdentifierOrKeyword()
{
	auto start = pos.get();
	// auto last = pos;

	do {
		pos.addvanceChar();
	} while (not isEnd() and isIdentifierOrKeywordInnerChar(getChar()) );

	std::string content = src.substr(start.index(), (pos - start).index());
	if (isKeyword(content)) {
		this->current = Token(TokenKind::KEYWORD, content, start);
	} else if (cat::isAnyOf(content, "and", "or", "not")) {
		this->current = Token(TokenKind::OPERATOR, content, start);
	} else if (isBoolen(content)) {
		this->current = Token(TokenKind::BOOLEAN, content, start);
	} else {
		this->current = Token(TokenKind::IDENTIFIER, content, start);
	}
}

void Tokenizer::readNumber()
{
	const auto start = pos.get();
	// auto last = pos;

	do {
		pos.addvanceChar();
	} while (not isEnd() and isDecimal(getChar()));

	if (not isEnd() and isLetter(getChar())) {
		// we've got a Lexing error:
		throw (SyntaxError("Integer contains invalid character(s).", pos.get()));
	}

	std::string content = src.substr(start.index(), (pos - start).index());
	this->current = Token(TokenKind::NUMBER, content, start);
}

void Tokenizer::readString()
{
	pos.addvanceChar();
	const auto start = pos.get();
	while (not isEnd() and not (getChar() == '"')) {
		pos.addvanceChar();
	} ;

	std::string content = src.substr(start.index(), (pos - start).index());

	if (not isEnd()) {
		pos.addvanceChar();
	} else {
		// we've got a Lexing error:
		throw (SyntaxError("String has no closing '\"'.", pos.get()));
	}

	this->current = Token(TokenKind::STRING, content, start);
}

void Tokenizer::readSeparator()
{
	const auto start = pos.get();
	std::string content = std::string() + getChar();
	pos.addvanceChar();
	this->current = Token(TokenKind::SEPARATOR, content, start);
}

void Tokenizer::readOperator()
{
	const auto start = pos.get();
	char c = getChar();
	std::string content = std::string() + c;
	pos.addvanceChar();
	if (cat::isAnyOf(c, '>', '<', '=', '!') and not isEnd() and getChar() == '=') {
		content += '=';
		pos.addvanceChar();
	}

	if (cat::isAnyOf(c, '-') and not isEnd() and getChar() == '>') {
		content += '>';
		pos.addvanceChar();
		this->current = Token(TokenKind::SEPARATOR, content, start);
	} else {
		this->current = Token(TokenKind::OPERATOR, content, start);
	}
}

void Tokenizer::skipWhitespacesAndCommentsIfAny()
{
	while (not isEnd()) {
		if (isSpace(getChar())) {
			// white space, line feed, carriage return, etc :
			skipWhitespaces();
		} else if (not isEnd() and getChar() == '/' and not isNextEnd()){
			// check for comment:
			if (getNextChar() == '/') {
				// line comment:
				skipLineComment();
			} else if (getNextChar() == '*') {
				// block comment:
				skipBlockComment();
			} else {
				return;
			}
		} else {
			return;
		}

	}
}

bool Tokenizer::handleSingleNewLineIfAny()
{
	NGPL_ASSERT(not isEnd());
	if (getChar() == '\r') {
		if (not isNextEnd() and getNextChar() == '\n'){
			pos.addvanceNewLine(2);
		} else {
			pos.addvanceNewLine(1);
		}
		return true;
	} else if (getChar() == '\n') {
		pos.addvanceNewLine(1);
		return true;
	} else {
		return false;
	}
}

void Tokenizer::skipWhitespaces()
{
	NGPL_ASSERT(not isEnd() and isSpace(getChar()));
	do {
		if (not handleSingleNewLineIfAny()) {
			// it is a normal space, tab, \f, ...
			pos.addvanceChar();
		}
	} while (not isEnd() and isSpace(getChar()));
}

void Tokenizer::skipLineComment()
{
	NGPL_ASSERT(not isEnd() and getChar() == '/' and not isNextEnd() and getNextChar() == '/');
	pos.addvanceChar();
	pos.addvanceChar();
	while (not isEnd()) {

		if (handleSingleNewLineIfAny()) {
			return;
		} else {
			// it's any other char...
			pos.addvanceChar();
		}

	}
}

void Tokenizer::skipBlockComment()
{
	NGPL_ASSERT(not isEnd() and getChar() == '/' and not isNextEnd() and getNextChar() == '*');
	pos.addvanceChar();
	pos.addvanceChar();
	while (not isEnd()) {
		if (not handleSingleNewLineIfAny()) {
			const auto c = getChar();
			if (c == '*') {
				if (not isNextEnd() and getNextChar() == '/') {
					pos.addvanceChar();
					pos.addvanceChar();
					break;
				}
			} else if (c == '/') {
				// maybe nested block comment?
				if (not isNextEnd() and getNextChar() == '*') {
					skipBlockComment();
					continue;
				}
			}
			pos.addvanceChar();
		}
	}

}

}

namespace ngpl {  // Character Categories:

bool Tokenizer::isLetter(char c) const
{
	return false
		or cat::isBetween(c, 'a', 'z')
		or cat::isBetween(c, 'A', 'Z')
		or c == '_';
}

bool Tokenizer::isOctal(char c) const
{
	return cat::isBetween(c, '0', '7');
}

bool Tokenizer::isDecimal(char c) const
{
	return cat::isBetween(c, '0', '9');
}

bool Tokenizer::isHexaDecimal(char c) const
{
	return false
		or isDecimal(c)
		or cat::isBetween(c, 'a', 'f')
		or cat::isBetween(c, 'A', 'F');
}

bool Tokenizer::isSpace(char c) const
{
	return cat::isAnyOf(c, ' ', '\f', '\t', '\r', '\n');
}

bool Tokenizer::isSeparator(char c) const
{
	return cat::isAnyOf(c, '(', ')', '{', '}', '[', ']', ';', ',', '.');
}

bool Tokenizer::isOperator(char c) const
{
	return cat::isAnyOf(c, '+', '-', '*', '/', '%', '@', '^', '&', '|', '!', '<', '>', '=', '?', ':');
}

bool Tokenizer::isIdentifierOrKeywordOpeningChar(char c) const
{
	return isLetter(c);
}

bool Tokenizer::isIdentifierOrKeywordInnerChar(char c) const
{
	return false
		or isLetter(c)
		or isDecimal(c);
}

bool Tokenizer::isKeyword(const std::string& s) const
{
	return cat::isAnyOf(
		s,
		"var",
		"let",
		"do",
		"if",
		"else",
		"when",
		"return",
		"func",
		"type",
		"struct",
		"class",
		"protocol",
		"mixin",
		"enum"
	) or cat::isAnyOf(
		s,
		"unit",
		"program",
		"package",
		"library"

	);
}

bool Tokenizer::isBoolen(const std::string& s) const
{
	return cat::isAnyOf(s, "true", "false");
}

}


namespace ngpl {  // LookAheadIterator:

const LookAheadIterator::T& LookAheadIterator::get() const
{
	if (lookAheads.empty()) {
		return src.get();
	} else {
		return lookAheads.front();
	}
}

void LookAheadIterator::advance()
{
	if (_isSimulating) {
		simulationHistory.push(get());
	}

	if (lookAheads.empty()) {
		src.advance();
	} else {
		lookAheads.pop();
	}
}

void LookAheadIterator::startSimulation()
{
	NGPL_ASSERT(_isSimulating == false);
	_isSimulating = true;
}

void LookAheadIterator::discardSimulation()
{
	NGPL_ASSERT(_isSimulating == true);
	lookAheads = std::move(simulationHistory);
	simulationHistory = {};
	_isSimulating = false;
}

void LookAheadIterator::acceptSimulation()
{
	NGPL_ASSERT(_isSimulating == true);
	simulationHistory = {};
	_isSimulating = false;
}

}


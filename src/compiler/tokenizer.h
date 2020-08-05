#ifndef TOKENIZER_H
#define TOKENIZER_H

#include "../language/token.h"
#include "../language/position.h"

#include "ranges.h"
#include "toStringUtils.h"

#include <string>
#include <queue>

namespace ngpl {

class Tokenizer : public cat::_CatIterator<Token, Tokenizer> {
public:
	using T = Token;
	using CatIterator = _CatIterator<Token, Tokenizer>;

protected:
	std::string src;
	MutablePosition pos;

	Token current;
	Token last;

	inline size_t length() const { return src.length(); }

	inline bool isEnd(uint64_t index) const { return index >= length(); }
	inline bool isEnd() const { return isEnd(pos.index()); }
	inline bool isNextEnd() const { return isEnd(pos.index() + 1); }

	/**
	 * @brief getChar get the curent char from src. WARNING: No array bound checks!
	 * @return src[pos.index()]
	 */
	inline char getChar() const {
	return src[pos.index()];
	}

	/**
	 * @brief getNextChar get the next char from src. WARNING: No array bound checks!
	 * @return src[pos.index() + 1]
	 */
	inline char getNextChar() const {
	return src[pos.index() + 1];
	}

public:
	Tokenizer(const std::string& src): CatIterator(), src(src), pos() {
	advance(); // go to first token
	};
	Tokenizer(const std::string& src, MutablePosition &&pos): CatIterator(), src(src), pos(std::move(pos)) {
	// advance(); do not to first token bc. pos already is somewhere in the src string.
	};

//    auto begin() const { return StdIterator(*this); };
	auto end() const { return Tokenizer("", MutablePosition(0, 0, length() + 1)); };

	bool isPastEnd() const {
	return pos.index() > src.size();
	}


	T const& get () const { return current; }

	void advance();


	bool operator ==(const Tokenizer& rhs ) const {
	return (this->pos.index() == rhs.pos.index()); // and (this->last == rhs.last) and (this->step == rhs.step);
	}

	bool operator !=(const Tokenizer& rhs ) const {
	return not (*this == rhs);
	}

	// Tokenizer specific methods:
	Position getPos() const { return pos.get(); }
	void readIdentifierOrKeyword();
	void readNumber();
	void readString();
	void readSeparator();
	void readOperator();
	void skipWhitespacesAndCommentsIfAny();
	bool handleSingleNewLineIfAny();
	void skipWhitespaces();
	void skipLineComment();
	void skipBlockComment();

	// Character Categories:
	bool isLetter(char c) const;
	bool isOctal(char c) const;
	bool isDecimal(char c) const;
	bool isHexaDecimal(char c) const;
	bool isSpace(char c) const;
	bool isSeparator(char c) const;
	bool isOperator(char c) const;


	bool isIdentifierOrKeywordOpeningChar(char c) const ;
	bool isIdentifierOrKeywordInnerChar(char c) const ;

	bool isKeyword(const std::string& s) const;
	bool isBoolen(const std::string& s) const;
};



class LookAheadIterator : public cat::_CatIterator<Token, LookAheadIterator> {
public:
	using T = Token;
	using CatIterator = _CatIterator<Token, LookAheadIterator>;

protected:
	Tokenizer src;

	bool _isSimulating = false;
	std::queue<T> lookAheads;
	std::queue<T> simulationHistory;


//    inline bool isEnd(uint64_t index) const { return index >= length(); }
//    inline bool isEnd() const { return isEnd(pos.index()); }
//    inline bool isNextEnd() const { return isEnd(pos.index() + 1); }


public:
	LookAheadIterator(Tokenizer&& src)
	: CatIterator(), src(std::move(src))
	{}

	auto end() const { return LookAheadIterator(src.end()); };

	bool isPastEnd() const {
	return src.isPastEnd() and lookAheads.empty();
	}


	const T& get () const;

	void advance();
	//void simulateAdvance();
	void startSimulation();
	void discardSimulation();
	void acceptSimulation();


	bool operator ==(const LookAheadIterator& rhs ) const {
	return (this->src == rhs.src) and (this->lookAheads == rhs.lookAheads); // and (this->last == rhs.last) and (this->step == rhs.step);
	}

	bool operator !=(const LookAheadIterator& rhs ) const {
	return not (*this == rhs);
	}

	// Tokenizer specific methods:
	Position getPos() const { return lookAheads.empty() ? src.getPos() : lookAheads.front().pos; }

};

}

#endif // TOKENIZER_H

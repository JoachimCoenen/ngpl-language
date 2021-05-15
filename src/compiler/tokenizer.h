#ifndef TOKENIZER_H
#define TOKENIZER_H

#include "../language/token.h"
#include "../language/position.h"

#include "ranges.h"
#include "cat_string.h"
#include "toStringUtils.h"

#include <queue>

namespace ngpl {

class Tokenizer : public cat::_CatIterator<Token, Tokenizer> {
public:
	using T = Token;
	using CatIterator = _CatIterator<Token, Tokenizer>;

protected:
	cat::String src;
	MutablePosition pos;
	Position currentTokenEndPos;

	Token current;

	inline size_t length() const { return src.length(); }
	inline size_t index() const { return pos.index(); }

	/**
	 * @brief is index past last character?
	 * @param index
	 * @return
	 */
	inline bool isPastLastChar(uint64_t index) const { return index >= length(); }
	inline bool isPastLastChar() const { return isPastLastChar(index()); }
	inline bool isNextPastLastChar() const { return isPastLastChar(index() + 1); }

	/**
	 * @brief getChar get the curent char from src. WARNING: No array bound checks!
	 * @return src[index()]
	 */
	inline char getChar() const {
		return src[index()];
	}

	/**
	 * @brief getNextChar get the next char from src. WARNING: No array bound checks!
	 * @return src[index() + 1]
	 */
	inline char getNextChar() const {
		return src[index() + 1];
	}

public:
	Tokenizer(const cat::String& src): CatIterator(), src(src), pos() {
		skipWhitespacesAndCommentsIfAny();
		advance(); // go to first token
	};
	Tokenizer(const cat::String& src, MutablePosition &&pos): CatIterator(), src(src), pos(std::move(pos)) {
		// advance(); do not to first token bc. pos already is somewhere in the src string.
	};

//    auto begin() const { return StdIterator(*this); };
	auto end() const { return Tokenizer("", MutablePosition(0, 0, length() + 1)); };

	inline bool isPastEnd() const { return index() > length();}


	T const& get () const { return current; }

	void advance();


	bool operator ==(const Tokenizer& rhs ) const {
		return (this->index() == rhs.index()); // and (this->last == rhs.last) and (this->step == rhs.step);
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

	bool isKeyword(const cat::String& s) const;
	bool isBoolen(const cat::String& s) const;
	bool isNil(const cat::String& s) const;
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

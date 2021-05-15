#ifndef POSITION_H
#define POSITION_H

#include "cat_string.h"
#include "toStringUtils.h"

#include <stdint.h>

namespace ngpl {


struct Position {
public:
	Position() {};
	Position(uint64_t line, uint64_t column, uint64_t index)
	: _line(line), _column(column), _index(index) {}

	inline const uint64_t& line() const { return _line; }
	inline const uint64_t& column() const { return _column; }
	inline const uint64_t& index() const { return _index; }
protected:
	uint64_t _line = 0;
	uint64_t _column = 0;
	uint64_t _index = 0;
};

inline Position operator - (const Position& lhs, const Position& rhs) {
	return Position(
	lhs.line()   - rhs.line(),
	lhs.column() - rhs.column(),
	lhs.index()  - rhs.index()
	);
}

inline Position operator + (const Position& lhs, const Position& rhs) {
	return Position(
	lhs.line()   + rhs.line(),
	lhs.column() + rhs.column(),
	lhs.index()  + rhs.index()
	);
}

inline bool operator < (const Position& lhs, const Position& rhs) {
	return lhs.index() < rhs.index();
}

inline bool operator <= (const Position& lhs, const Position& rhs) {
	return lhs.index() <= rhs.index();
}

inline bool operator == (const Position& lhs, const Position& rhs) {
	return lhs.index() == rhs.index();
}


inline bool operator > (const Position& lhs, const Position& rhs) {
	return rhs < lhs;
}

inline bool operator >= (const Position& lhs, const Position& rhs) {
	return rhs <= lhs;
}

inline bool operator != (const Position& lhs, const Position& rhs) {
	return not (lhs == rhs);
}

struct MutablePosition: public Position {
public:
	MutablePosition() : Position() {};
	MutablePosition(uint64_t line, uint64_t column, uint64_t index) : Position( line, column, index) {}

	Position get() const {
	return Position(line(), column(), index());
	}

	// inline void addCR() {
	//     _line += 1;
	//     _column = 0;
	//     _index += 1;
	//     // _lastWasCR = true;
	// }

	// inline void addLF() {
	//     _index += 1;
	//     if (_lastWasCR) {
	//         _lastWasCR = false;
	//     } else {
	//         _line += 1;
	//         _column = 0;
	//     }
	// }

	inline void addvanceChar() {
	++_column;
	++_index;
	// _lastWasCR = false;
	}
	inline void addvanceChars(uint64_t count) {
	_column += count;
	_index += count;
	// _lastWasCR = false;
	}


	inline void addvanceNewLine(uint_fast8_t chars) {  // to accoutn for CR+LF = only one new line
	_line += 1;
	_column = 0;
	_index += chars;  // to accoutn for CR+LF = only one new line
	}

protected:
	//bool _lastWasCR = false;

};



namespace _ngpl_internal_ {
inline cat::WriterObjectABC& _writePositionBody (cat::WriterObjectABC& s, const Position& v) {
	auto tuple = std::make_tuple(
	MEMBER_PAIR_GET(v, line),
	MEMBER_PAIR_GET(v, column),
	MEMBER_PAIR_GET(v, index)
	);
	formatTupleLike2(s, tuple, {"(", ")"}, cat::_formatFuncKwArg, true);
	return s;
}
}

inline cat::WriterObjectABC& operator+= (cat::WriterObjectABC& s, const Position& v) {
	s += "Position";
	return _ngpl_internal_::_writePositionBody(s, v);
}

inline cat::WriterObjectABC& operator+= (cat::WriterObjectABC& s, const MutablePosition& v) {
	s += "MutablePosition";
	return _ngpl_internal_::_writePositionBody(s, v);
}

}
#endif // POSITION_H

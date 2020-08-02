#include "token.h"


namespace ngpl {

cat::WriterObjectABC& operator +=(cat::WriterObjectABC& s, const TokenKind& v) {
	switch (v) {
   FORMAT_ENUM_VAL_CASE(TokenKind, KEYWORD);
   FORMAT_ENUM_VAL_CASE(TokenKind, IDENTIFIER);
   FORMAT_ENUM_VAL_CASE(TokenKind, OPERATOR);
   FORMAT_ENUM_VAL_CASE(TokenKind, SEPARATOR);
   FORMAT_ENUM_VAL_CASE(TokenKind, NUMBER);
   FORMAT_ENUM_VAL_CASE(TokenKind, STRING);
   FORMAT_ENUM_VAL_CASE(TokenKind, BOOLEAN);
   FORMAT_ENUM_VAL_CASE(TokenKind, COMMENT);
	}
	return s;
}

cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const Token& v) {
	s += "Token";
	auto tuple = std::make_tuple(
	MEMBER_PAIR(v, kind),
	MEMBER_PAIR(v, content),
	MEMBER_PAIR(v, pos)
	);
	formatTupleLike2(s, tuple, {"(", ")"}, cat::_formatFuncKwArg, true);
	return s;
}

}

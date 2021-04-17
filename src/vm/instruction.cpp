#include "instruction.h"

namespace ngpl {

//const Position Instruction::_pos = Position();

cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const Instruction& v) {
	s += "Token";
	auto tuple = std::make_tuple(
	MEMBER_PAIR_GET(v, id),
	MEMBER_PAIR_GET(v, data),
	MEMBER_PAIR_GET(v, pos)
	);
	formatTupleLike2(s, tuple, {"(", ")"}, cat::_formatFuncKwArg, true);
	return s;
}

cat::WriterObjectABC& Instruction::print(cat::WriterObjectABC& s) const
{
	const auto idStr = cat::String(cat::SW() << _id);
	s += idStr;
	s += cat::String(std::max(2ll, 20 - int64_t(idStr.length())), ' ');

	const auto dataStr = cat::formatVal(_data);
	s += dataStr;
	s += cat::String(std::max(2ll, 35 - int64_t(dataStr.length())), ' ');
	s +=  _pos.line();
	return s;
}

cat::String Instruction::toString() const
{
	auto s = cat::SW();
	print(s);
	return s;
}

}


















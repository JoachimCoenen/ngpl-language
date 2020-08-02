#include "instruction.h"


namespace ngpl {

//const Position Instruction::_pos = Position();

const Instructions Instructions::stackDeltaForInstructions{};

#define INSTRUCTION_FACTORY0(instrName, stackDelta, funcName) FORMAT_ENUM_VAL_CASE(InstructionID, instrName);
#define INSTRUCTION_FACTORY1(instrName, stackDelta, funcName, argType, arg) FORMAT_ENUM_VAL_CASE(InstructionID, instrName);
cat::WriterObjectABC& operator +=(cat::WriterObjectABC& s, const InstructionID& v) {
	switch (v) {
#include "instructions_inc.h"
	}
	throw cat::Exception("instruction ID not handeled!" + std::to_string(int(v)));
}
#undef INSTRUCTION_FACTORY0
#undef INSTRUCTION_FACTORY1

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
	const auto idStr = std::string(cat::SW() << _id);
	s += idStr;
	s += std::string(std::max(2ll, 20 - int64_t(idStr.length())), ' ');

	const auto dataStr = cat::formatVal(_data);
	s += dataStr;
	s += std::string(std::max(2ll, 35 - int64_t(dataStr.length())), ' ');
	s +=  _pos.line();
	return s;
}

std::string Instruction::toString() const
{
	auto s = cat::SW();
	print(s);
	return s;
}

#define INSTRUCTION_FACTORY0(instrName, stackDelta, funcName) array[static_cast<uint8_t>(InstructionID::instrName)] = stackDelta;
#define INSTRUCTION_FACTORY1(instrName, stackDelta, funcName, argType, arg) array[static_cast<uint8_t>(InstructionID::instrName)] = stackDelta;
Instructions::Instructions()
{
	#include "instructions_inc.h"
}

#undef INSTRUCTION_FACTORY0
#undef INSTRUCTION_FACTORY1

}

















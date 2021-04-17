#include "intermediateCode.h"

#include "compiler/language/function.h"

#include "toStringUtils.h"

namespace ngpl::intermediate {

IntermediateInstruction::IntermediateInstruction()
{

}

IntermediateSimpleInstruction::IntermediateSimpleInstruction(InstructionID id,
		const SimpleInstructionData& data,
		const Position& pos
)
	: IntermediateInstruction(),
	  _id(id),
	  _data(data),
	  _pos(pos)
{}

bool IntermediateSimpleInstruction::hasSideEffect() const {
	if (id() == InstructionID::CALL) {
		auto func = FunctionBaseCWeakPtr(static_cast<const FunctionBase*>(data().template getValue<const void*>()));
		return func->hasSideEffect();
	}


#define INSTRUCTION_FACTORY0(instrName, stackDelta, hasSideEffect, funcName) \
	case InstructionID::instrName: return hasSideEffect;
#define INSTRUCTION_FACTORY1(instrName, stackDelta, hasSideEffect, funcName, argType, arg)\
	case InstructionID::instrName: return hasSideEffect;

	switch (id()) {
#include "util/instructions_inc.h"
	}
	throw cat::Exception(cat::SW() << "instruction ID not handeled! " << id());
#undef INSTRUCTION_FACTORY0
#undef INSTRUCTION_FACTORY1
}

cat::WriterObjectABC& IntermediateSimpleInstruction::print(cat::WriterObjectABC& s) const
{
	s += cat::nlIndent;
	const auto idStr = cat::String(cat::SW() << _id);
	s += idStr;
	s += cat::String(std::max(2ll, 20 - int64_t(idStr.length())), ' ');

	auto dataStr = cat::formatVal(_data);
	if (id() == InstructionID::CALL) {
		dataStr = static_cast<const FunctionBase*>(data().getValue<const void*>())->asQualifiedCodeString();
	}
	s += dataStr;
	s += cat::String(std::max(2ll, 35 - int64_t(dataStr.length())), ' ');
	s +=  _pos.line();
	return s;
}

cat::String IntermediateSimpleInstruction::toString() const
{
	auto s = cat::SW();
	print(s);
	return s;
}

cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const IntermediateSimpleInstruction& v) {
	s += "Token";
	auto dataStr = cat::formatVal(v.data());
	if (v.id() == InstructionID::CALL) {
		dataStr = static_cast<const FunctionBase*>(v.data().getValue<const void*>())->asQualifiedCodeString();
	}
	auto tuple = std::make_tuple(
	MEMBER_PAIR_GET(v, id),
	cat::makeCRefPair("data", dataStr),
	MEMBER_PAIR_GET(v, pos)
	);
	formatTupleLike2(s, tuple, {"(", ")"}, cat::_formatFuncKwArg, true);
	return s;
}


const Instructions Instructions::stackDeltaForInstructions{};

#define INSTRUCTION_FACTORY0(instrName, stackDelta, hasSideEffect, funcName) array[static_cast<uint8_t>(InstructionID::instrName)] = stackDelta;
#define INSTRUCTION_FACTORY1(instrName, stackDelta, hasSideEffect, funcName, argType, arg) array[static_cast<uint8_t>(InstructionID::instrName)] = stackDelta;
Instructions::Instructions()
{
	#include "util/instructions_inc.h"
}
#undef INSTRUCTION_FACTORY0
#undef INSTRUCTION_FACTORY1


IntermediateLoop::IntermediateLoop(IntermediateCodeContainer&& code, const Position& pos)
	: IntermediateInstruction(),
	  code(std::move(code)),
	  _pos(pos)
{}

cat::WriterObjectABC& IntermediateLoop::print(cat::WriterObjectABC& s) const
{
	s += cat::nlIndent;
	s += "loop:";
	s.incIndent();
	code.print(s);
	s.decIndent();
	return s;
}

bool IntermediateLoop::hasSideEffect() const
{
	return code.hasSideEffect();
}

IntermediateIf::IntermediateIf(bool isInverted, IntermediateCodeContainer&& ifCode, IntermediateCodeContainer&& elseCode, const Position& pos)
	: IntermediateInstruction(),
	  isInverted(isInverted),
	  ifCode(std::move(ifCode)),
	  elseCode(std::move(elseCode)),
	  _pos(pos)
{}

cat::WriterObjectABC& IntermediateIf::print(cat::WriterObjectABC& s) const
{
	s += cat::nlIndent;
	s += isInverted ? "if not:" : "if:";
	s.incIndent();
	ifCode.print(s);
	s.decIndent();
	if (not elseCode.instructions.empty()) {
		s += cat::nlIndent;
		s += "else:";
		s.incIndent();
		elseCode.print(s);
		s.decIndent();
	}
	return s;
}

bool IntermediateIf::hasSideEffect() const
{
	return ifCode.hasSideEffect() or elseCode.hasSideEffect();
}

cat::WriterObjectABC& operator +=(cat::WriterObjectABC& s, const IntermediateSpecialId& v)
{
	switch (v) {
		FORMAT_ENUM_VAL_CASE(IntermediateSpecialId, RETURN);
		FORMAT_ENUM_VAL_CASE(IntermediateSpecialId, CONTINUE);
		FORMAT_ENUM_VAL_CASE(IntermediateSpecialId, BREAK);
	}
	throw cat::Exception("IntermediateSpecialId " + std::to_string(int(v)) + " not handeled!");
}

IntermediateSpecial::IntermediateSpecial(IntermediateSpecialId id, const Position& pos)
	: IntermediateInstruction(),
	  id(id),
	  _pos(pos)
{}

cat::WriterObjectABC& IntermediateSpecial::print(cat::WriterObjectABC& s) const
{
	s += cat::nlIndent;
	switch (id) {
	case IntermediateSpecialId::RETURN:
		s += "return";
		break;
	case IntermediateSpecialId::CONTINUE:
		s += "continue";
		break;
	case IntermediateSpecialId::BREAK:
		s += "break";
		break;
	}
	return s;
}

cat::WriterObjectABC& IntermediateCodeContainer::print(cat::WriterObjectABC& s) const
{
	foreach_c(instr, instructions) {
		instr->print(s);
	}
	return s;
}

bool IntermediateCodeContainer::hasSideEffect() const
{
	foreach_c(instr, instructions) {
		if (instr->hasSideEffect()) {
			return true;
		}
	}
	return false;
}

bool IntermediateCodeContainer::isEmpty() const
{
	return this->instructions.empty();
}

}

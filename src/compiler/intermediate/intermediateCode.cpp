#include "intermediateCode.h"

#include "toStringUtils.h"

namespace ngpl::intermediate {

IntermediateCode::IntermediateCode()
{

}

IntermediateInstruction::IntermediateInstruction(Instruction&& instr)
	: IntermediateCode(),
	  instr(std::move(instr))
{}

cat::WriterObjectABC& IntermediateInstruction::print(cat::WriterObjectABC& s) const
{
	s += cat::nlIndent;
	instr.print(s);
	return s;
}

IntermediateLoop::IntermediateLoop(IntermediateCodeContainer&& code, const Position& pos)
	: IntermediateCode(),
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

IntermediateIf::IntermediateIf(bool isInverted, IntermediateCodeContainer&& ifCode, IntermediateCodeContainer&& elseCode, const Position& pos)
	: IntermediateCode(),
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
	: IntermediateCode(),
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

}

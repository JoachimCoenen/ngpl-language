#ifndef INTERMEDIATECODE_H
#define INTERMEDIATECODE_H

#include "../../vm/instruction.h"
#include "../../util/types.h"
//#include "cat_variant.h"

#include <vector>


namespace cat {
class WriterObjectABC;
}

namespace ngpl::intermediate {

PTRS_FOR_CLASS(IntermediateCode)
class IntermediateCode: public IIntermediateCodePrintable
{
protected:
	IntermediateCode();
public:
	virtual ~IntermediateCode() {};
	virtual const Position& pos() const = 0;
};



PTRS_FOR_CLASS(IntermediateCodeContainer)
class IntermediateCodeContainer: public IIntermediateCodePrintable {
public:
	std::vector<IntermediateCodePtr> instructions;
	// InstructionsContainer
	cat::WriterObjectABC& print(cat::WriterObjectABC& s) const override;
};


PTRS_FOR_CLASS(IntermediateInstruction)
class IntermediateInstruction: public IntermediateCode {
public:
	IntermediateInstruction(Instruction&& instr);

	Instruction instr;

	cat::WriterObjectABC& print(cat::WriterObjectABC& s) const override;
	const Position& pos() const override { return instr.pos(); }
};


PTRS_FOR_CLASS(IntermediateLoop)
class IntermediateLoop: public IntermediateCode {
public:
	IntermediateLoop(IntermediateCodeContainer&& code, const Position& pos);

	IntermediateCodeContainer code;

	cat::WriterObjectABC& print(cat::WriterObjectABC& s) const override;
	const Position& pos() const override { return _pos; }

protected:
	Position _pos;
};


PTRS_FOR_CLASS(IntermediateIf)
class IntermediateIf: public IntermediateCode {
public:
	IntermediateIf(bool isInverted, IntermediateCodeContainer&& ifCode, IntermediateCodeContainer&& elseCode, const Position& pos);

	bool isInverted;
	IntermediateCodeContainer ifCode;
	IntermediateCodeContainer elseCode;

	cat::WriterObjectABC& print(cat::WriterObjectABC& s) const override;
	const Position& pos() const override { return _pos; }

protected:
	Position _pos;
};


enum class IntermediateSpecialId {
	RETURN,
	CONTINUE,
	BREAK
};
cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const IntermediateSpecialId& v);


PTRS_FOR_CLASS(IntermediateSpecial)
class IntermediateSpecial: public IntermediateCode {
public:
	IntermediateSpecial(IntermediateSpecialId id, const Position& pos);

	IntermediateSpecialId id;

	cat::WriterObjectABC& print(cat::WriterObjectABC& s) const override;
	const Position& pos() const override { return _pos; }

protected:
	Position _pos;
};

}

#endif // INTERMEDIATECODE_H

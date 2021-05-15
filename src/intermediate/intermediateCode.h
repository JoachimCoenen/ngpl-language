#ifndef INTERMEDIATECODE_H
#define INTERMEDIATECODE_H

#include "../compiler/compileError.h"
#include "language/position.h"
#include "util/instructionID.h"
#include "util/types.h"

#include "cat_string.h"
#include "cat_variant.h"

#include <vector>


namespace cat {
class WriterObjectABC;
}

namespace ngpl::intermediate {

PTRS_FOR_CLASS(IntermediateInstruction);
class IntermediateInstruction: public IIntermediateCodePrintable
{
protected:
	IntermediateInstruction();
public:
	virtual ~IntermediateInstruction() {};
	virtual const Position& pos() const = 0;
	virtual bool hasSideEffect() const = 0;

};
cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const IntermediateInstruction& v);



PTRS_FOR_CLASS(IntermediateCodeContainer);
class IntermediateCodeContainer: public IIntermediateCodePrintable {
public:
	// InstructionsContainer
	cat::WriterObjectABC& print(cat::WriterObjectABC& s) const override;
	bool hasSideEffect() const;

	bool isEmpty() const;

	void addError(CompileErrorPtr&& error);
	const std::vector<CompileErrorPtr>& errors() const { return _compileErrors; }
	bool isBad() const { return not _compileErrors.empty(); }

protected:
	void printErrorList(cat::WriterObjectABC& s) const;

public:
	std::vector<IntermediateInstructionPtr> instructions;

private:
	std::vector<CompileErrorPtr> _compileErrors;
};


PTRS_FOR_CLASS(IntermediateSimpleInstruction);
class IntermediateSimpleInstruction: public IntermediateInstruction {
public:
	IntermediateSimpleInstruction(InstructionID id,
		const InstructionData& data,
		const Position& pos
	);
	InstructionID id() const { return _id; }
	const InstructionData& data() const { return _data; }
	const Position& pos() const override final { return _pos; }
	bool hasSideEffect() const override final;


	cat::WriterObjectABC& print(cat::WriterObjectABC& s) const override final;
	cat::String toString() const;
protected:
	InstructionID _id;
	// cat::String holds the name of a variable.
	// void* is a non-owning ptr to a ngpl::Function object.
	InstructionData _data;
	Position _pos;

	friend class Instructions;
};
cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const IntermediateSimpleInstruction& v);


class Instructions {
private:
	std::array<int8_t, 256> array;

	Instructions();
	Instructions(const Instructions&) = delete;
	Instructions(Instructions&&) = delete;
	Instructions& operator =(const Instructions&) = delete;
	Instructions& operator =(Instructions&&) = delete;

public:

	int8_t operator[](InstructionID index) const {
		return array[static_cast<uint8_t>(index)];
	}

	static const Instructions stackDeltaForInstructions;
	//int64_t getStackDeltaForInstruction();

#define INSTRUCTION_FACTORY0(instrName, stackDelta, hasSideEffect, funcName)		\
	static auto funcName(const Position& pos) {       \
	return IntermediateSimpleInstruction(InstructionID::instrName, None_{}, pos); \
	}

#define INSTRUCTION_FACTORY1(instrName, stackDelta, hasSideEffect, funcName, argType, arg)		\
	static auto funcName(argType arg, const Position& pos) {   \
		return IntermediateSimpleInstruction(InstructionID::instrName, arg, pos);           \
	}
	#include "util/instructions_inc.h"
#undef INSTRUCTION_FACTORY0
#undef INSTRUCTION_FACTORY1
};


PTRS_FOR_CLASS(IntermediateLoop)
class IntermediateLoop: public IntermediateInstruction {
public:
	IntermediateLoop(IntermediateCodeContainer&& code, const Position& pos);

	IntermediateCodeContainer code;

	cat::WriterObjectABC& print(cat::WriterObjectABC& s) const override;
	const Position& pos() const override final { return _pos; }
	bool hasSideEffect() const override final;

protected:
	Position _pos;
};


PTRS_FOR_CLASS(IntermediateIf)
class IntermediateIf: public IntermediateInstruction {
public:
	IntermediateIf(bool isInverted, IntermediateCodeContainer&& ifCode, IntermediateCodeContainer&& elseCode, const Position& pos);

	bool isInverted;
	IntermediateCodeContainer ifCode;
	IntermediateCodeContainer elseCode;

	cat::WriterObjectABC& print(cat::WriterObjectABC& s) const override;
	const Position& pos() const override final { return _pos; }
	bool hasSideEffect() const override final;

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
class IntermediateSpecial: public IntermediateInstruction {
public:
	IntermediateSpecial(IntermediateSpecialId id, const Position& pos);

	IntermediateSpecialId id;

	cat::WriterObjectABC& print(cat::WriterObjectABC& s) const override;
	const Position& pos() const override final { return _pos; }
	bool hasSideEffect() const override final { return false; };

protected:
	Position _pos;
};

}

#endif // INTERMEDIATECODE_H

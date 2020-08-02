#ifndef INSTRUCTION_H
#define INSTRUCTION_H

#include "../language/position.h"

#include "vm_util.h"
#include "../util/types.h"

#include "cat_variant.h"

#include <string>

namespace ngpl {

struct None_ {};
inline cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const None_&) {
	return s += "   ";
}

#define INSTRUCTION_FACTORY0(instrName, stackDelta, funcName) instrName,
#define INSTRUCTION_FACTORY1(instrName, stackDelta, funcName, argType, arg) instrName,
enum class InstructionID {
#include "instructions_inc.h"

};
#undef INSTRUCTION_FACTORY0
#undef INSTRUCTION_FACTORY1
cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const InstructionID& v);


struct Instruction: public IIntermediateCodePrintable {
	InstructionID id() const { return _id; }
	// std::string holds the name of a variable.
	// void* is a non-owning ptr to a ngpl::Function object.
	const cat::Variant<None_, std::string, int64_t, const void*>& data() const { return _data; }

	cat::WriterObjectABC& print(cat::WriterObjectABC& s) const override final;
	std::string toString() const;


	const Position& pos() const { return _pos; }

	Instruction(InstructionID id, const cat::Variant<None_, std::string, int64_t, const void*>& data, const Position& pos)
		: _id(id),
		  _data(data),
		  _pos(pos)
	{}
protected:
	InstructionID _id;
	// std::string holds the name of a variable.
	// void* is a non-owning ptr to a ngpl::Function object.
	cat::Variant<None_, std::string, int64_t, const void*> _data;
	Position _pos;

	friend class Instructions;
};
cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const Instruction& v);





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

#define INSTRUCTION_FACTORY0(instrName, stackDelta, funcName)		\
	static auto funcName(const Position& pos) {       \
	return Instruction(InstructionID::instrName, None_{}, pos); \
	}

#define INSTRUCTION_FACTORY1(instrName, stackDelta, funcName, argType, arg)		\
	static auto funcName(argType arg, const Position& pos) {   \
		return Instruction(InstructionID::instrName, arg, pos);           \
	}
	#include "instructions_inc.h"
};
#undef INSTRUCTION_FACTORY0
#undef INSTRUCTION_FACTORY1

}
#endif // INSTRUCTION_H

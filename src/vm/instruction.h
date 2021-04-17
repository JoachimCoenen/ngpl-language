#ifndef INSTRUCTION_H
#define INSTRUCTION_H

#include "../language/position.h"

#include "vm_util.h"
#include "util/instructionID.h"
#include "util/types.h"

#include "cat_string.h"
#include "cat_variant.h"

namespace ngpl {

struct Instruction: public IIntermediateCodePrintable {
	InstructionID id() const { return _id; }
	// cat::String holds the name of a variable.
	// void* is a non-owning ptr to a ngpl::Function object.
	const cat::Variant<None_, cat::String, int64_t, const void*>& data() const { return _data; }

	cat::WriterObjectABC& print(cat::WriterObjectABC& s) const override final;
	cat::String toString() const;


	const Position& pos() const { return _pos; }

	Instruction(InstructionID id, const cat::Variant<None_, cat::String, int64_t, const void*>& data, const Position& pos)
		: _id(id),
		  _data(data),
		  _pos(pos)
	{}
protected:
	InstructionID _id;
	// cat::String holds the name of a variable.
	// void* is a non-owning ptr to a ngpl::Function object.
	cat::Variant<None_, cat::String, int64_t, const void*> _data;
	Position _pos;

	friend class Instructions;
};
cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const Instruction& v);


namespace instructions {

#define INSTRUCTION_FACTORY0(instrName, stackDelta, hasSideEffect, funcName)		\
	static inline auto funcName(const Position& pos) {       \
	return Instruction(InstructionID::instrName, None_{}, pos); \
	}

#define INSTRUCTION_FACTORY1(instrName, stackDelta, hasSideEffect, funcName, argType, arg)		\
	static inline auto funcName(argType arg, const Position& pos) {   \
		return Instruction(InstructionID::instrName, arg, pos);           \
	}
	#include "util/instructions_inc.h"
};
#undef INSTRUCTION_FACTORY0
#undef INSTRUCTION_FACTORY1

}
#endif // INSTRUCTION_H

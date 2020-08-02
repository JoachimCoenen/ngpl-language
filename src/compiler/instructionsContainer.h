#ifndef SCOPEDINSTRUCTIONSCONTAINER_H
#define SCOPEDINSTRUCTIONSCONTAINER_H

#include "../vm/instruction.h"
#include "../util/types.h"

#include "cat_typing.h"
#include "toStringUtils.h"
#include "ranges.h"

namespace ngpl {

struct Scope;

PTRS_FOR_STRUCT(InstructionsContainer)
struct InstructionsContainer: public IIntermediateCodePrintable {
	InstructionsContainer() {}
	std::vector<Instruction> instructions;


	virtual bool isGlobal() const {
		return false;
	}

	cat::WriterObjectABC& print(cat::WriterObjectABC& s) const override {
		foreach_c(instr, instructions) {
			s += cat::nlIndent;
			instr.print(s);
		}
		return s;
	}

};

}

#endif // SCOPEDINSTRUCTIONSCONTAINER_H

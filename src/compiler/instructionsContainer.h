#ifndef SCOPEDINSTRUCTIONSCONTAINER_H
#define SCOPEDINSTRUCTIONSCONTAINER_H

#include "intermediate/intermediateCode.h"
#include "../vm/instruction.h"
#include "../util/types.h"

#include "cat_typing.h"
#include "toStringUtils.h"
#include "ranges.h"

namespace ngpl {

struct Scope;

PTRS_FOR_STRUCT(InstructionsContainer)
struct InstructionsContainer: public intermediate::IntermediateCodeContainer {
	InstructionsContainer()
	: intermediate::IntermediateCodeContainer()
{}

//	virtual bool isGlobal() const {
//		return false;
//	}

};

}

#endif // SCOPEDINSTRUCTIONSCONTAINER_H

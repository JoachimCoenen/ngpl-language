#include "instructionID.h"

#include "cat_exception.h"
#include "toStringUtils.h"

namespace ngpl {

#define INSTRUCTION_FACTORY0(instrName, stackDelta, hasSideEffect, funcName) FORMAT_ENUM_VAL_CASE(InstructionID, instrName);
#define INSTRUCTION_FACTORY1(instrName, stackDelta, hasSideEffect, funcName, argType, arg) FORMAT_ENUM_VAL_CASE(InstructionID, instrName);
cat::WriterObjectABC& operator +=(cat::WriterObjectABC& s, const InstructionID& v) {
	switch (v) {
#include "instructions_inc.h"
	}
	throw cat::Exception("instruction ID not handeled! " + std::to_string(int(v)));
}
#undef INSTRUCTION_FACTORY0
#undef INSTRUCTION_FACTORY1

}

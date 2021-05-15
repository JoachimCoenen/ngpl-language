#ifndef INSTRUCTIONID_H
#define INSTRUCTIONID_H

namespace cat {
// forward decl:
class WriterObjectABC;
}

namespace ngpl {

#define INSTRUCTION_FACTORY0(instrName, stackDelta, hasSideEffect, funcName) instrName,
#define INSTRUCTION_FACTORY1(instrName, stackDelta, hasSideEffect, funcName, argType, arg) instrName,
enum class InstructionID {
#include "instructions_inc.h"

};
#undef INSTRUCTION_FACTORY0
#undef INSTRUCTION_FACTORY1
cat::WriterObjectABC& operator += (cat::WriterObjectABC& s, const InstructionID& v);




}


#endif // INSTRUCTIONID_H

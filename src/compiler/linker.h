#ifndef LINKER_H
#define LINKER_H

#include "unit.h"

#include "../vm/instruction.h"

#include <string>
#include <unordered_map>
#include <vector>

namespace ngpl {

class Linker
{
public:
	Linker(std::vector<UnitCWeakPtr>&& units);

	void generateInstructionStream();
	void generateInstructionStream(InstructionsContainerCWeakPtr container);
	void generateInstructionStream(cat::WeakPtr<const Scope> scope);

	void linkFunctions();


	std::vector<UnitCWeakPtr> _units;
	std::vector<Instruction> _instructions;
	std::unordered_map<std::string, Address> _functionEntryPoints;
};

}

#endif // LINKER_H

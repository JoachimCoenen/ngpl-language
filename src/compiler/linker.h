#ifndef LINKER_H
#define LINKER_H

#include "unit.h"

#include "intermediate/intermediateCode.h"
#include "../vm/instruction.h"

#include <string>
#include <unordered_map>
#include <vector>

namespace {
namespace itm = ngpl::intermediate;
}

namespace ngpl {

class Linker
{
public:
	Linker(std::vector<UnitCWeakPtr>&& units);

	void generateInstructionStream();
	void generateInstructionStream(itm::IntermediateCodeContainerCWeakPtr container);
	void generateInstructionStream(ScopeCWeakPtr scope);

	void generateInstructionStreamIf(itm::IntermediateIfCWeakPtr ifInstr);
	void generateInstructionStreamLoop(intermediate::IntermediateLoopCWeakPtr loopInstr);
	void generateInstructionStreamSpecial(intermediate::IntermediateSpecialCWeakPtr specialInstr);

	void linkFunctions();


	std::vector<UnitCWeakPtr> _units;
	std::vector<Instruction> _instructions;
	std::unordered_map<std::string, Address> _functionEntryPoints;

	//cat::Stack<std::vector<std::pair<size_t, itm::IntermediateSpecialId>>> _funcControls;
	cat::Stack<std::vector<std::pair<size_t, itm::IntermediateSpecialId>>> _loopControls;
};

}

#endif // LINKER_H
